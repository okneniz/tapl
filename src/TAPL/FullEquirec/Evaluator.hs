{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPL.FullEquirec.Evaluator (evalString) where

import TAPL.FullEquirec.Types
import TAPL.FullEquirec.Parser
import TAPL.FullEquirec.Names
import TAPL.FullEquirec.TypeChecker

import Data.List (findIndex, intercalate, all, nub, (\\), last)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Text.Parsec (ParseError)

evalString :: String -> String -> Either String String
evalString code path =
    case parse path code of
        (Left e) -> Left $ "Parse error : " ++ show e
        (Right (names, ast)) ->
            case eval $ EvaluationContext names ast of
                (Right (EvaluationContext names' ast')) ->
                    let t = last ast'
                    in case typeOf names' t of
                            Right ty ->
                                Right $ show (EvaluationContext names' t) ++ ":" ++ show (EvaluationContext names' ty)
                            Left e ->
                                Left $ show (EvaluationContext names' e)
                (Left e) -> Left $ show e

class (Normalizeable (c n t), LCTypeChecker n) => TypedEvaluator c n t where
    eval :: c n t -> Either (c n TypeError) (c n t)

class Normalizeable a where
    normalize :: a ->  Maybe a

data EvaluationContext n t = EvaluationContext n t

instance TypedEvaluator EvaluationContext Names [Term] where
    eval (EvaluationContext names (t@(TBind _ _ _):ast)) = do
        case eval $ EvaluationContext names t of
            Right (EvaluationContext names' t') ->
                let types = (typeOf names') <$> ast
                    errors = filter isLeft types
                    firstError = case head errors of Left e -> EvaluationContext names' e
                in if (length errors) == 0
                   then if (length ast) > 0
                        then eval $ EvaluationContext names' ast
                        else Right $ EvaluationContext names' [t']
                   else Left $ firstError
            Left e -> Left $ e

    eval (EvaluationContext names (x:xs)) = do
        case normalize $ EvaluationContext names x of
            Just (EvaluationContext names' x') -> eval $ EvaluationContext names' (x':xs)
            Nothing | length xs > 0 -> eval $ EvaluationContext names xs
            Nothing -> Right $ EvaluationContext names [x]

    eval x = error $ show x

instance Show (EvaluationContext Names [Term]) where
    show (EvaluationContext ns ts) = show ts

instance TypedEvaluator EvaluationContext Names Term where
    eval c@(EvaluationContext names t) =
        case normalize c of
            Just c' -> eval c'
            Nothing -> Right c

instance Normalizeable (EvaluationContext Names [Term]) where
    normalize (EvaluationContext names (x:xs)) =
        case normalize $ EvaluationContext names x of
            (Just ( EvaluationContext names' x')) -> Just $ EvaluationContext names' (x':xs)
            Nothing -> Just $ EvaluationContext names xs

    normalize (EvaluationContext names []) = Nothing

instance Normalizeable (EvaluationContext Names Term) where
    normalize (EvaluationContext ns (TIf _ (TTrue _) t _ )) = return $ EvaluationContext ns t
    normalize (EvaluationContext ns (TIf _ (TFalse _) _ t)) = return $ EvaluationContext ns t

    normalize (EvaluationContext ns (TIf info t1 t2 t3)) = do
      (EvaluationContext ns t1') <- normalize $ EvaluationContext ns t1
      return $ EvaluationContext ns (TIf info t1' t2 t3)

    normalize (EvaluationContext ns (TApp _ (TAbs _ _ _ t) v)) | isVal v =
        return $ EvaluationContext ns (termSubstitutionTop v t)

    normalize (EvaluationContext ns (TApp info t1 t2)) | isVal t1  = do
        (EvaluationContext ns t2') <- normalize $ EvaluationContext ns t2
        return $ EvaluationContext ns (TApp info t1 t2')

    normalize (EvaluationContext ns (TApp info t1 t2)) = do
        (EvaluationContext ns t1') <- normalize $ EvaluationContext ns t1
        return $ EvaluationContext ns (TApp info t1' t2)

    normalize (EvaluationContext ns (TSucc info t)) = do
        (EvaluationContext ns t') <- normalize $ EvaluationContext ns t
        return $ EvaluationContext ns (TSucc info t')

    normalize (EvaluationContext ns (TPred _ (TZero info))) =
        return $ EvaluationContext ns (TZero info)

    normalize (EvaluationContext ns (TPred _ (TSucc _ t))) | isNumerical t =
        return $ EvaluationContext ns t

    normalize (EvaluationContext ns (TPred info t)) = do
        (EvaluationContext ns t') <- normalize $ EvaluationContext ns t
        return $ EvaluationContext ns (TPred info t')

    normalize (EvaluationContext ns (TIsZero _ (TZero info))) =
        return $ EvaluationContext ns (TTrue info)

    normalize (EvaluationContext ns (TIsZero _ (TSucc info t))) | isNumerical t =
        return $ EvaluationContext ns (TFalse info)

    normalize (EvaluationContext ns (TIsZero info t)) = do
        (EvaluationContext ns t') <- normalize $ EvaluationContext ns t
        return $ EvaluationContext ns (TIsZero info t')

    normalize (EvaluationContext ns (TPair info t1 t2)) | isVal t1  = do
        (EvaluationContext ns t2') <- normalize $ EvaluationContext ns t2
        return $ EvaluationContext ns (TPair info t1 t2')

    normalize c@(EvaluationContext ns (TPair info t1 t2)) = do
        (EvaluationContext ns t1') <- normalize $ EvaluationContext ns t1
        return $ EvaluationContext ns (TPair info t1' t2)

    normalize c@(EvaluationContext ns (TRecord _ [])) = Nothing
    normalize c@(EvaluationContext ns t@(TRecord _ fields)) | isVal t = Nothing

    normalize (EvaluationContext ns (TRecord info fields)) = do
        return $ EvaluationContext ns (TRecord info fields')
         where fields' = foldl normalizeField [] fields
               normalizeField fs (k, t) =
                    case normalize $ EvaluationContext ns t of
                         (Just (EvaluationContext ns t')) -> normalizeField fs (k, t')
                         _ -> fs ++ [(k, t)]

    normalize (EvaluationContext ns (TLookup _ (TPair _ t _) (TInt _ 0))) | isVal t =
        return $ EvaluationContext ns t

    normalize (EvaluationContext ns (TLookup _ (TPair _ _ t) (TInt _ 1))) | isVal t =
        return $ EvaluationContext ns t

    normalize (EvaluationContext ns (TLookup info t@(TRecord _ fields) (TKeyword _ key))) | isVal t = do
        case Prelude.lookup key fields of
             Just t -> return $ EvaluationContext ns t
             Nothing -> error "evaluation error"

    normalize (EvaluationContext ns (TLookup info t k)) = do
        (EvaluationContext ns t') <- normalize $ EvaluationContext ns t
        return $ EvaluationContext ns (TLookup info t' k)

    normalize (EvaluationContext ns (TLet info v t1 t2)) | isVal t1 =
        return $ EvaluationContext ns (termSubstitutionTop t1 t2)

    normalize (EvaluationContext ns (TLet info v t1 t2)) = do
        (EvaluationContext ns t1') <- normalize $ EvaluationContext ns t1
        return $ EvaluationContext ns (TLet info v t1' t2)

    normalize (EvaluationContext ns (TAscribe info t ty)) | isVal t = do
        return $ EvaluationContext ns t

    normalize (EvaluationContext ns (TAscribe info t ty)) = do
        (EvaluationContext ns t') <- normalize $ EvaluationContext ns t
        return $ EvaluationContext ns t'

    normalize (EvaluationContext ns t1@(TFix _ a@(TAbs _ _ _ t2))) | isVal a = do
        return $ EvaluationContext ns $ termSubstitutionTop t1 t2

    normalize (EvaluationContext ns (TFix info t)) = do
        (EvaluationContext ns' t') <- normalize $ EvaluationContext ns t
        return $ EvaluationContext ns' (TFix info t')

    normalize (EvaluationContext ns (TTimesFloat info (TFloat _ t1) (TFloat _ t2))) = do
        return $ EvaluationContext ns (TFloat info (t1 * t2))

    normalize (EvaluationContext ns (TTimesFloat info t1@(TFloat _ _) t2)) = do
        (EvaluationContext ns t2') <- normalize $ EvaluationContext ns t2
        return $ EvaluationContext ns (TTimesFloat info t1 t2')

    normalize (EvaluationContext ns (TTimesFloat info t1 t2@(TFloat _ _))) = do
        (EvaluationContext ns t1') <- normalize $ EvaluationContext ns t1
        return $ EvaluationContext ns (TTimesFloat info t1' t2)

    normalize (EvaluationContext ns (TCase _ (TTag _ key v _) branches)) | isVal v = do
      (_, t) <- lookup key $ (\(key, varName, t) -> (key, (varName, t))) <$> branches
      return $ EvaluationContext ns $ termSubstitutionTop v t

    normalize (EvaluationContext ns (TCase info t fields)) = do
      (EvaluationContext ns t') <- normalize $ EvaluationContext ns t
      return $ EvaluationContext ns (TCase info t' fields)

    normalize (EvaluationContext ns (TBind info name binding)) = do
      return $ (EvaluationContext (bind ns name binding) (TUnit info))

    normalize _ = Nothing

instance Show (EvaluationContext Names TypeError) where
    show (EvaluationContext ns (ArgumentError info expected actual)) =
        "Invalid arguament type : expected " ++ show expected ++ ", actual " ++ show actual

    show (EvaluationContext ns (DifferentTypesInBranches info tys)) =
        "Branches of condition have different types : " ++ (intercalate ", " $ show <$> tys)

    show (EvaluationContext ns (InvalidGuardType info ty)) =
        "Guard of condition hasn't a Bool type : " ++ (show $ EvaluationContext ns ty)

    show (EvaluationContext ns (WrongKindOfBinding info ty)) =
        "Wrong kind of binding for variable"

    show (EvaluationContext ns (VariableNotFound info varname)) =
        "Variable " ++ show varname ++ " not found"

    show (EvaluationContext ns (IncorrectApplication info t1 t2)) =
        "Incorrect application " ++ (show $ t1) ++ " and " ++ (show $ t2) ++ (show ns)

    show (EvaluationContext ns (InvalidIndex info i)) =
        "Invalid index for pair : " ++ show i

    show (EvaluationContext ns (InvalidKeyword info key t)) =
        "Invalid keyword " ++ show key ++ " for " ++ show t

    show (EvaluationContext ns (UnexpectedOperation info t)) =
        "Unexpected operation " ++ (show $ EvaluationContext ns t)

    show (EvaluationContext ns (TypesMissmatch info ty1 ty2)) =
        "Types missmatch " ++ show ty1 ++ " and " ++ show ty2

    show (EvaluationContext ns (IncompatibilityType info ty expected)) =
        "Incomparability type " ++ show ty ++ " expected " ++ show expected

    show (EvaluationContext ns (UnexpectedType info ty)) =
        "Unexpected type in " ++ show info ++ " " ++ (show $ EvaluationContext ns ty)

    show (EvaluationContext ns (NotImplementedCaseBranches info keys)) =
        "Case branch with key not implemented " ++ intercalate "," keys

    show (EvaluationContext ns (MissmatchVariantKey info k)) =
        "Missmatch variant with key " ++ show k

    show (EvaluationContext ns (InvalidCaseBranches info fs)) =
        "Invalid case branches " ++ intercalate ", " fs

    show (EvaluationContext ns (InvalidCaseBranchesTypes info fs)) =
        "Invalid case branch types " ++ intercalate ", " ((\(Left x) -> show x) <$> fs)

    show (EvaluationContext ns (CaseBranchesHaveDifferentTypes info tys)) =
        "Case branches have different types " ++ (intercalate ", " $ show <$> tys)

    show (EvaluationContext ns (LabelNotFound info key)) =
        "Label " ++ key ++ " not found"

    show (EvaluationContext ns (InvalidFixation info)) =
        "Invalid fixation " ++ show info

    show (EvaluationContext ns InvalidAST) = "Invalid ast"

instance Show (EvaluationContext Names Term) where
    show (EvaluationContext _ (TTrue _)) = "true"
    show (EvaluationContext _ (TFalse _)) = "false"
    show (EvaluationContext _ (TString _ s)) = show s
    show (EvaluationContext _ (TUnit _)) = "unit"
    show (EvaluationContext _ (TZero _)) = "zero"
    show (EvaluationContext _ (TFloat _ t)) = show t
    show (EvaluationContext _ (TInt _ x)) = show x
    show (EvaluationContext _ (TKeyword _ x)) = x
    show (EvaluationContext n (TSucc _ t)) = "succ " ++ (show $ EvaluationContext n t)
    show (EvaluationContext n (TPred _ t)) = "pred " ++ (show $ EvaluationContext n t)
    show (EvaluationContext n (TIsZero _ t)) = "zero? " ++ (show $ EvaluationContext n t)
    show (EvaluationContext n (TBind _ x (TypeAddBind ty))) = x ++ " = " ++ (show $ EvaluationContext n ty)

    show (EvaluationContext n (TIf _ t1 t2 t3)) =
        "if " ++ (show $ EvaluationContext n t1) ++
        " then " ++ (show $ EvaluationContext n t2) ++
        " else " ++ (show $ EvaluationContext n t3)

    show (EvaluationContext n (TVar _ varName _)) =
        case findName n varName of
             Just x -> x
             Nothing -> "[wtf?] " ++ show n ++ " "

    show (EvaluationContext n (TAbs _ name _ t)) =
        let (name', n') = pickFreshName n name
        in "(lambda " ++ name' ++ "." ++ (show $ EvaluationContext n' t)  ++ ")"

    show (EvaluationContext n (TApp _ t1 t2)) =
        (show $ EvaluationContext n t1) ++ " " ++ (show $ EvaluationContext n t2)

    show (EvaluationContext n (TPair _ t1 t2)) =
        "{" ++ (show $ EvaluationContext n t1) ++ "," ++ (show $ EvaluationContext n t2) ++ "}"

    show (EvaluationContext n (TRecord _ ts)) =
        "{" ++ (intercalate ", " $ (\(k,ty) -> k ++ "=" ++ (show $ EvaluationContext n ty)) <$> ts) ++ "}"

    show (EvaluationContext n (TLookup _ t k)) =
        (show $ EvaluationContext n t) ++ "." ++ (show $ EvaluationContext n k)

    show (EvaluationContext n (TLet _ v t1 t2)) =
        "let " ++ v ++ " = " ++ show (EvaluationContext n t1) ++ " in " ++ show (EvaluationContext n t2)

    show (EvaluationContext ns (TTag _ key t ty)) =
        "<" ++ key ++ "=" ++ show (EvaluationContext ns t) ++ ">"

    show (EvaluationContext n (TAscribe _ t ty)) =
        (show $ EvaluationContext n t) ++ ":" ++ (show $ EvaluationContext n ty)

    show (EvaluationContext n (TCase _ t cases)) =
        "case " ++ (show $ EvaluationContext n t) ++ " of " ++
        intercalate " | " (df <$> cases)
        where df (caseName, varName, t) = "<" ++ caseName ++ "=" ++ varName ++ "> -> " ++ show (EvaluationContext n t)

    show (EvaluationContext ns (TFix _ t)) = show (EvaluationContext ns t)
    show (EvaluationContext ns (TTimesFloat _ t1 t2)) =
        "timesfloat" ++ (show $ EvaluationContext ns t1) ++ (show $ EvaluationContext ns t2)

instance Show (EvaluationContext Names Type) where
    show (EvaluationContext n TyBool) = "Bool"
    show (EvaluationContext n TyString) = "String"
    show (EvaluationContext n TyTop) = "Top"
    show (EvaluationContext n TyBot) = "Bot"
    show (EvaluationContext n TyUnit) = "Unit"
    show (EvaluationContext n TyNat) = "Nat"
    show (EvaluationContext n TyInt) = "Int"
    show (EvaluationContext n TyFloat) = "Float"
    show (EvaluationContext n TyKeyword)= "Keyword"
    show (EvaluationContext n (TyID s)) = s

    show (EvaluationContext n (TyArrow ty1 ty2)) =
        "(" ++ (show $ EvaluationContext n ty1) ++ " -> " ++ (show $ EvaluationContext n ty2) ++ ")"

    show (EvaluationContext n (TyProduct t1 t2)) =
        "{" ++ (show $ EvaluationContext n t1) ++ "*" ++ (show $ EvaluationContext n t2) ++ "}"

    show (EvaluationContext n (TyRecord ts)) =
        "{" ++ (intercalate ", " $ (\(k,ty) -> k ++ "=" ++ (show $ EvaluationContext n ty)) <$> ts) ++ "}"

    show (EvaluationContext n (TyVariant ts)) = "<" ++ (intercalate ", " $ map field ts) ++ ">"
                    where field (k,t) = k ++ ":" ++ (show $ EvaluationContext n t)

    show (EvaluationContext n (TyRec s ty)) =
        let (s', n') = pickFreshName n s in
        "Rec " ++ s' ++ "." ++ (show $ EvaluationContext n' ty)

    show (EvaluationContext n (TyVar varName ty)) =
        case findName n varName of
             Just x -> x
             Nothing -> "[wtf?] " ++ show varName ++ " in " ++ show n ++ " ? "
