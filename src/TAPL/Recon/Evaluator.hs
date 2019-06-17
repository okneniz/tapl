{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPL.Recon.Evaluator (evalString) where

import TAPL.Recon.Types
import TAPL.Recon.Parser
import TAPL.Recon.Context
import TAPL.Recon.TypeChecker

import Data.List (findIndex, intercalate, all, nub, (\\), last, foldl1)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Text.Parsec (ParseError)

evalString :: String -> String -> Either String String
evalString code path =
    case parse path code of
        (Left e) -> Left $ "Parse error : " ++ show e
        (Right (names, ast)) ->
            case check $ Context names newUVarGen [] ast of
                Right (Context names' vars' constraints' tys) ->
                    case eval $ Context names' vars' constraints' ast of
                        (Right (Context names'' vars'' constraints'' ast')) ->
                            let c = Context names'' vars'' constraints'' $ last ast'
                            in case typeOf c of
                                    Right ty -> Right $ show c ++ ":" ++ show ty
                                    Left e -> Left $ show e
                        (Left e) -> Left $ show e
                (Left e) -> Left $ show e

class (Normalizeable (c t), LCTypeChecker c) => TypedEvaluator c t where
    eval :: c t -> Either (c EvaluationError) (c t)
    check :: c t -> Either (c EvaluationError) (c t)

class Normalizeable a where
    normalize :: a ->  Maybe a

instance TypedEvaluator Context [Term] where
    check (Context names vars constraints []) =
        Left $ Context names vars constraints AttemptToEvaluateEmptyAST

    check (Context names vars constraints ast) = do
        let c = Context names vars constraints ast
            types = scanl f (typeOf $ Context names vars constraints (head ast)) (tail ast)
            f (Right (Context ns' vs' cs' _)) x = typeOf $ Context ns' vs' cs' x
            f (Left x) _ = Left x
            errors = filter isLeft types
        case errors of
             [] -> case last types of
                        (Right (Context names' vars' constraints' tys')) ->
                            return $ Context names' vars' constraints' ast
             ((Left e):_) -> Left e

    eval c@(Context names vars constraints []) = do
        Left $ Context names vars constraints AttemptToEvaluateEmptyAST

    eval (Context names vars constraints (t@(TBind _ _ _):ast)) = do
        (Context names' vars' constraints' _) <- eval $ Context names vars constraints t
        c <- check $ Context names' vars' constraints' ast
        return c

    eval c@(Context names vars constraints [x]) = do
        (Context names vars constraints x') <- eval $ Context names vars constraints x
        return $ Context names vars constraints [x']

    eval (Context names vars constraints (x:xs)) = do
        (Context names' vars' constraints' x') <- eval $ Context names vars constraints x
        return $ Context names' vars' constraints' (x':xs)

instance Show (Context [Term]) where
    show (Context ns vs cs ts) = intercalate "; " $ (\t -> show $ Context ns vs cs t) <$> ts

instance TypedEvaluator Context Term where
    eval c@(Context names vars constraints t) =
        case normalize c of
             Just c' -> eval c'
             Nothing -> Right c

instance Normalizeable (Context [Term]) where
    normalize (Context names vars constraints (x:xs)) =
        case normalize $ Context names vars constraints x of
             (Just (Context names' vars' constraints' x')) -> Just $ Context names' vars' constraints' (x':xs)
             Nothing -> Just $ Context names vars constraints xs

    normalize (Context names vars constraints []) = Nothing

instance Normalizeable (Context Term) where
    normalize (Context ns vs cs (TIf _ (TTrue _) t _ )) = return $ Context ns vs cs t
    normalize (Context ns vs cs (TIf _ (TFalse _) _ t)) = return $ Context ns vs cs t

    normalize (Context ns vs cs (TIf info t1 t2 t3)) = do
      (Context ns vs cs t1') <- normalize $ Context ns vs cs t1
      return $ Context ns vs cs (TIf info t1' t2 t3)

    normalize (Context ns vs cs (TApp _ (TAbs _ _ _ t) v)) | isVal v =
        return $ Context ns vs cs (termSubstitutionTop v t)

    normalize (Context ns vs cs (TApp info t1 t2)) | isVal t1  = do
        (Context ns vs cs t2') <- normalize $ Context ns vs cs t2
        return $ Context ns vs cs (TApp info t1 t2')

    normalize (Context ns vs cs (TApp info t1 t2)) = do
        (Context ns vs cs t1') <- normalize $ Context ns vs cs t1
        return $ Context ns vs cs (TApp info t1' t2)

    normalize (Context ns vs cs (TSucc info t)) = do
        (Context ns vs cs t') <- normalize $ Context ns vs cs t
        return $ Context ns vs cs (TSucc info t')

    normalize (Context ns vs cs (TPred _ (TZero info))) =
        return $ Context ns vs cs (TZero info)

    normalize (Context ns vs cs (TPred _ (TSucc _ t))) | isNumerical t =
        return $ Context ns vs cs t

    normalize (Context ns vs cs (TPred info t)) = do
        (Context ns vs cs t') <- normalize $ Context ns vs cs t
        return $ Context ns vs cs (TPred info t')

    normalize (Context ns vs cs (TIsZero _ (TZero info))) =
        return $ Context ns vs cs (TTrue info)

    normalize (Context ns vs cs (TIsZero _ (TSucc info t))) | isNumerical t =
        return $ Context ns vs cs (TFalse info)

    normalize (Context ns vs cs (TIsZero info t)) = do
        (Context ns vs cs t') <- normalize $ Context ns vs cs t
        return $ Context ns vs cs (TIsZero info t')

    normalize (Context ns vs cs (TBind info name binding)) = do
      return $ (Context (bind ns name binding) vs cs (TTrue info)) -- TODO

    normalize _ = Nothing

instance Show (Context Term) where
    show (Context _ _ _ (TTrue _)) = "true"
    show (Context _ _ _ (TFalse _)) = "false"
    show (Context _ _ _ (TZero _)) = "zero"
    show (Context n v c (TSucc _ t)) = "succ " ++ (show $ Context n v c t)
    show (Context n v c (TPred _ t)) = "pred " ++ (show $ Context n v c t)
    show (Context n v c (TIsZero _ t)) = "zero? " ++ (show $ Context n v c t)
    show (Context n v c (TBind _ x (VarBind ty))) = x ++ " = " ++ (show $ Context n v c ty)

    show (Context n v c (TIf _ t1 t2 t3)) =
        "if " ++ (show $ Context n v c t1) ++
        " then " ++ (show $ Context n v c t2) ++
        " else " ++ (show $ Context n v c t3)

    show (Context n _ _ (TVar _ varName _)) =
        case findName n varName of
             Just x -> x
             Nothing -> "[wtf?] " ++ show n ++ " "

    show (Context n v c (TAbs _ name _ t)) =
        let (name', n') = pickFreshName n name
        in "(lambda " ++ name' ++ "." ++ (show $ Context n' v c t)  ++ ")"

    show (Context n v c (TApp _ t1 t2)) =
        (show $ Context n v c t1) ++ " " ++ (show $ Context n v c t2)

instance Show (Context Type) where
    show (Context _ _ _ TyBool) = "Bool"
    show (Context _ _ _ TyNat) = "Nat"
    show (Context _ _ _ (TyID s)) = s

    show (Context n v c (TyArrow ty1 ty2)) =
        "(" ++ (show $ Context n v c ty1) ++ " -> " ++ (show $ Context n v c ty2) ++ ")"

    show (Context n v c (TyVar varName ty)) =
        case findName n varName of
             Just x -> x
             Nothing -> "[wtf?] " ++ show varName ++ " in " ++ show n ++ " ? "

instance Show (Context EvaluationError) where
    show (Context _ _ _ (WrongKindOfBinding info varname)) = "Wrong type of binding"
    show (Context _ _ _ (CircularConstrains varname)) = "Circular constraints"
    show (Context ns vs cs (UnresolvedConstraints ((ty1, ty2):_))) =
        "Type missmatch " ++ (show $ Context ns vs cs ty1) ++ " and " ++ (show $ Context ns vs cs ty2)
    show (Context _ _ _ AttemptToEvaluateEmptyAST) = "Attempt to evaluate empty AST"
