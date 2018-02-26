{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullSimple where

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intercalate, all, nub, (\\))
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

type VarName = Int
type Depth = Int

data Info = Info { row :: Int, column :: Int } deriving (Eq)

data Term = TTrue Info
          | TFalse Info
          | TIf Info Term Term Term
          | TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          | TString Info String
          | TFloat Info Float
          | TUnit Info
          | TZero Info
          | TSucc Info Term
          | TPred Info Term
          | TIsZero Info Term
          | TPair Info Term Term
          | TRetrieve Info Term Int
          | TRecord Info [(String, Term)]
          | TLookup Info Term String
          | TLet Info String Term Term
          | TAscribe Info Term Type
          | TVariant Info String Term
          | TCase Info Term [(Pattern, Term)]
          | TFix Info Term
          deriving (Show, Eq)

data Type = TyBool
          | TyArrow Type Type
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyProduct Type Type
          | TyRecord [(String, Type)]
          | TyID String
          | TyVariant [(String, Type)]
          deriving (Eq)

data Binding = NameBind
             | VarBind Type
             deriving (Show)

type Context = [(String,Binding)]

data Pattern = Pattern String String deriving (Show, Eq)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

instance Show Type where
    show (TyArrow t1 t2) = "(" ++ show t1 ++ "->" ++ show t2 ++ ")"
    show TyBool = "Bool"
    show TyString = "String"
    show TyUnit = "Unit"
    show TyNat = "Nat"
    show TyFloat = "Float"
    show (TyProduct t1 t2) = show t1 ++ " x " ++ show t2
    show (TyRecord ts) = "{" ++ (intercalate "," $ map field ts) ++ "}"
                   where field (k,t) = k ++ "=" ++ show t
    show (TyID s) = s
    show (TyVariant ts) = "<" ++ (intercalate "," $ map field ts) ++ ">"
                    where field (k,t) = k ++ ":" ++ show t
-- Context

bind :: String -> Binding -> Context -> Context
bind x b ctx = (x,b):ctx

addName :: String -> Context -> Context
addName x ctx = bind x NameBind ctx

isNameBound :: String -> Context -> Bool
isNameBound name [] = False
isNameBound name ((x,_):xs) | x == name = True
isNameBound name ((_,_):xs) = isNameBound name xs

pickFreshName :: String -> Context -> (String, Context)
pickFreshName name context | isNameBound name context = pickFreshName (name ++ "'") context
pickFreshName name context = (name, (addName name context))

pickVar :: Context -> VarName -> Maybe (String, Binding)
pickVar [] name = Nothing
pickVar context name | length context > name = Just $ context !! name
pickVar _ _ = Nothing

nameFromContext :: Context -> VarName -> Maybe String
nameFromContext context name = liftM fst $ pickVar context name

typeFromContext :: Context -> VarName -> Maybe Binding
typeFromContext context name = liftM snd $ pickVar context name

display :: Context -> Term -> String
display _ (TTrue _) = "true"
display _ (TFalse _) = "false"
display c (TIf info t1 t2 t3) = "if " ++ display c t1 ++ " then " ++ display c t2 ++ " else " ++ display c t3
display c v@(TVar _ name depth) =
    let s = nameFromContext c name
    in case s of
            Just s -> s
            _ -> "[bad index in " ++ show v ++ " in context " ++ show c  ++ "]"

display context (TAbs info name ty x) =
    let (name', context') = pickFreshName name context
    in "(λ" ++ name' ++ "." ++ (display context' x) ++ ")"

display context (TApp info x y) = (display context x) ++ (display context y) ++ ":" ++ (show $ typeof context $ TApp info x y)

display context (TString info s) = s
display context (TUnit info) = "unit"
display context (TZero info) = "0"
display context (TFloat info t) = show t
display context (TSucc info t) = "succ " ++ (display context t)
display context (TPred info t) = "pred " ++ (display context t)
display context (TIsZero info t) = "isZero " ++ (display context t)
display context (TPair info t1 t2) = "{" ++ display context t1 ++ "," ++ display context t2 ++ "}"
display context (TRetrieve info t n) = display context t ++ "." ++ show n

display context (TRecord info ts) = "{" ++ (intercalate "," $ map field ts) ++ "}"
                              where field (x,y) = x ++ "=" ++ display context y

display context (TLookup info (TRecord _ fields) k) = display context $ fromJust $ lookup k fields

display context (TVariant info key t) = "<" ++ key ++ ":" ++ display context t ++ ">"
display context (TAscribe info t ty) =  display context t ++ " as " ++ show ty

display context (TCase info t fields) = "case " ++ display context t ++ " of "
                                                ++ intercalate " | " (df <$> fields)
                                  where df (Pattern pk varname, t) = "<" ++ pk ++ ":" ++ varname ++ "> -> " ++ display (c' pk varname) t
                                        vty = fromRight $ (\(TyVariant fs) -> fs) <$> typeof context t
                                        c' pk v = bind v (VarBind $ fromJust $ lookup pk vty) context

display context (TFix _ t) = display context t
display context t = show context ++ " ??? " ++ show t

-- Parsing

parse' :: String -> Either ParseError (Context, [Term])
parse' = runParser (expressions <* eof) defaultContext "simple typed λ-calculus"
   where defaultContext = []
         expressions = do
           ts <- terms
           c <- getState
           return (c, ts)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parens :: Parsec String Context a -> Parsec String Context a
parens = between (char '(') (char ')')

padded :: Stream s m Char => String -> ParsecT s u m String
padded x = spaces *> string x <* spaces

terms :: Parsec String Context [Term]
terms = term `sepEndBy` op
        where  def = []
               op = do
                 padded ";"
                 optionMaybe newline

term :: Parsec String Context Term
term = try (ascribed x) <|> x where x = try app <|> try notApp <|> parens term

ascribed :: Parsec String Context Term -> Parsec String Context Term
ascribed x = do
    t <- x
    padded "as"
    ty <- typeAnnotation
    pos <- getPosition
    return $ TAscribe (infoFrom pos) t ty

app :: Parsec String Context Term
app = let x = ascribed y
          y = (try notApp) <|> try (parens notApp)
      in chainl1 (try x <|> try y) $ do
            void $ spaces
            pos <- getPosition
            return $ TApp (infoFrom pos)

notApp :: Parsec String Context Term
notApp = try boolean
     <|> try float
     <|> try string'
     <|> try unit
     <|> try nat
     <|> try zero
     <|> try succ
     <|> try pred
     <|> try isZero
     <|> try let'
     <|> try retrieve
     <|> try lookup'
     <|> try var
     <|> try condition
     <|> try pair
     <|> try abs
     <|> try record
     <|> try variant
     <|> try case'
     <|> try fix

abs :: Parsec String Context Term
abs = do
    char 'λ'
    v <- identifier
    vt <- termType
    modifyState $ bind v $ VarBind vt
    char '.'
    t <- term
    modifyState tail
    pos <- getPosition
    return $ TAbs (infoFrom pos) v vt t

var :: Parsec String Context Term
var = do
    name <- identifier
    context <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) context of
        Nothing -> fail $ "variable " ++ name ++ " has't been bound in context " ++ (show context) ++ " " ++ (show $ infoFrom pos)
        Just n -> return $ TVar (infoFrom pos) n (length context)

boolean :: Parsec String Context Term
boolean = try true <|> false

string' :: Parsec String Context Term
string' = do
    char '\"'
    s <- many letter
    char '\"'
    pos <- getPosition
    return $ TString (infoFrom pos) s

unit :: Parsec String Context Term
unit = do
    string "unit"
    pos <- getPosition
    return $ TUnit (infoFrom pos)

nat :: Parsec String Context Term
nat = zero <|> succ <|> pred

succ :: Parsec String Context Term
succ = do
    padded "succ"
    t <- term
    pos <- getPosition
    return $ TSucc (infoFrom pos) t

pred :: Parsec String Context Term
pred = do
    padded "pred"
    t <- term
    pos <- getPosition
    return $ TPred (infoFrom pos) t

zero :: Parsec String Context Term
zero = do
    padded "0"
    pos <- getPosition
    return $ TZero (infoFrom pos)

isZero :: Parsec String Context Term
isZero = do
    padded "isZero"
    t <- term
    pos <- getPosition
    return $ TIsZero (infoFrom pos) t

float :: Parsec String Context Term
float = do
    x <- many digit
    char '.'
    y <- many digit
    pos <- getPosition
    let f = read (x ++ "." ++ y) :: Float
    return $ TFloat (infoFrom pos) f

true :: Parsec String Context Term
true = do
    padded "true"
    pos <- getPosition
    return $ TTrue (infoFrom pos)

false :: Parsec String Context Term
false = do
    padded "false"
    pos <- getPosition
    return $ TFalse (infoFrom pos)

condition :: Parsec String Context Term
condition = do
    padded "if"
    x <- term
    padded "then"
    y <- term
    padded "else"
    z <- term
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

pair :: Parsec String Context Term
pair = do
    padded "{"
    t1 <- term
    padded ","
    t2 <- term
    padded "}"
    pos <- getPosition
    return $ TPair (infoFrom pos) t1 t2

retrieve :: Parsec String Context Term
retrieve = do
    t <- var <|> pair
    char '.'
    n <- oneOf ['0', '1']
    pos <- getPosition
    return $ TRetrieve (infoFrom pos) t (read [n] :: Int)

lookup' :: Parsec String Context Term
lookup' = do
    t <- var <|> record
    char '.'
    k <- many letter
    context <- getState
    guard (hasKey context t k) <?> "invalid key name"
    pos <- getPosition
    return $ TLookup (infoFrom pos) t k
    where hasKey c t k = case typeof c t of
                              (Right (TyRecord fields)) -> isJust $ lookup k fields
                              _ -> False

record :: Parsec String Context Term
record = do
    padded "{"
    ts <- (keyValue '=') `sepBy` (padded ",")
    padded "}"
    pos <- getPosition
    return $ TRecord (infoFrom pos) ts

variant :: Parsec String Context Term
variant = do
    padded "<"
    (k,t) <- keyValue ':'
    padded ">"
    pos <- getPosition
    return $ TVariant (infoFrom pos) k t

keyValue :: Char -> Parsec String Context (String, Term)
keyValue c = do
     k <- many letter
     char c
     v <- term
     return (k, v)

let' :: Parsec String Context Term
let' = do
    padded "let"
    v <- identifier
    padded "="
    t1 <- term
    context <- getState
    padded "in"
    let ty = typeof context t1
    guard (isRight ty) <?> "type missmatch"
    modifyState $ bind v $ VarBind (fromRight ty)
    t2 <- term
    pos <- getPosition
    return $ TLet (infoFrom pos) v t1 t2

case' :: Parsec String Context Term
case' = do
    padded "case"
    t <- term
    padded "of"
    context <- getState
    guard (isVariant context t) <?> "type missmatch"
    let fs = variantFields $ fromRight $ typeof context t
    ts <- (pattern fs) `sepBy` (padded "|")
    pos <- getPosition
    return $ TCase (infoFrom pos) t ts

fix :: Parsec String Context Term
fix = do
    padded "fix"
    t <- term
    pos <- getPosition
    return $ TFix (infoFrom pos) t

isVariant :: Context -> Term -> Bool
isVariant c t = case typeof c t of
                     Right (TyVariant _ ) -> True
                     _ -> False

variantFields :: Type -> [(String, Type)]
variantFields (TyVariant fields) = fields
variantFields _ = undefined

pattern :: [(String, Type)] -> Parsec String Context (Pattern, Term)
pattern fields = do
    char '<'
    variantName <- many letter
    char ':'
    varName <- identifier
    let varType = lookup variantName fields
    guard (isJust varType) <?> "variant type has't field " ++ varName
    char '>'
    padded "->"
    modifyState $ bind varName $ VarBind $ fromJust varType
    t <- term
    modifyState tail
    return (Pattern variantName varName, t)

identifier :: Parsec String Context String
identifier = do
    v <- oneOf ['a'..'z']
    s <- optionMaybe $ many $ char '\''
    case s of
        Just s' -> return $ v:s'
        Nothing -> return [v]

termType :: Parsec String Context Type
termType = do
    char ':'
    typeAnnotation

typeAnnotation :: Parsec String Context Type
typeAnnotation = try arrowAnnotation
             <|> try productAnnotation
             <|> try recordAnnotation
             <|> try variantAnnotation
             <|> notArrowAnnotation

arrowAnnotation :: Parsec String Context Type
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    padded "->"
    return $ TyArrow

productAnnotation :: Parsec String Context Type
productAnnotation = chainl1 notProductAnnotation $ do
    padded "x"
    return $ TyProduct

notProductAnnotation :: Parsec String Context Type
notProductAnnotation = parens arrowAnnotation <|> notArrowAnnotation

notArrowAnnotation :: Parsec String Context Type
notArrowAnnotation = try booleanAnnotation
                 <|> try stringAnnotation
                 <|> try unitAnnotation
                 <|> try natAnnotation
                 <|> try floatAnnotation
                 <|> try baseTypeAnnotation

booleanAnnotation :: Parsec String Context Type
booleanAnnotation = do
    string "Bool"
    return TyBool

stringAnnotation :: Parsec String Context Type
stringAnnotation = do
    string "String"
    return TyString

unitAnnotation :: Parsec String Context Type
unitAnnotation = do
    string "Unit"
    return TyUnit

natAnnotation :: Parsec String Context Type
natAnnotation = do
    string "Nat"
    return TyNat

floatAnnotation :: Parsec String Context Type
floatAnnotation = do
    string "Float"
    return TyFloat

baseTypeAnnotation :: Parsec String Context Type
baseTypeAnnotation = do
    i <- oneOf ['A'..'Z']
    d <- many $ oneOf ['a'..'z']
    return $ TyID (i:d)

recordAnnotation :: Parsec String Context Type
recordAnnotation = do
    string "{"
    ts <- (keyType '=') `sepBy` (padded ",")
    string "}"
    return $ TyRecord ts

variantAnnotation :: Parsec String Context Type
variantAnnotation = do
    string "<"
    ts <- (keyType ':') `sepBy` (padded ",")
    string ">"
    return $ TyVariant ts

keyType :: Char -> Parsec String Context (String, Type)
keyType c = do
     k <- many letter
     char c
     v <- typeAnnotation
     return (k, v)

-- Evaluation

eval :: String -> String
eval code = case parse' code of
                 Right (context, terms) ->
                    let typeCheck = typeof context
                        result = (\x -> (x, typeCheck x)) <$> terms
                        ff (t, Right x) = (display context $ f context t) ++ ":" ++ (show x)
                        ff (_, Left x) = x
                    in intercalate "\n" $ ff <$> result
                 Left x -> show x
      where f c t = case eval' c t of
                         Just t' -> f c t'
                         Nothing -> t

fromRight (Right x) = x
fromRight (Left x) = undefined

fromLeft (Left x) = x
fromLeft (Right x) = undefined

fromJust (Just x) = x
fromJust Nothing = undefined

eval' :: Context -> Term -> Maybe Term
eval' context (TIf _ (TTrue _) t _ ) = return t
eval' context (TIf _ (TFalse _) _ t) = return t
eval' context (TIf info t1 t2 t3) = liftM (\t1' -> TIf info t1' t2 t3) (eval' context t1)
eval' context (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t

eval' context (TApp info t1 t2) | isVal t1   = liftM2 (TApp info) (return t1) (eval' context t2)
                                | otherwise  = liftM2 (TApp info) (eval' context t1) (return t2)

eval' context (TSucc info x) = liftM (TSucc info) (eval' context x)
eval' context (TPred _ (TZero info)) = return $ TZero info
eval' context (TPred _ (TSucc _ x)) | isNumerical x = return x
eval' context (TPred info x) = liftM (TPred info) (eval' context x)
eval' context (TIsZero _ (TZero info)) = return $ TTrue info
eval' context (TIsZero _ (TSucc info x)) | isNumerical x = return $ TFalse info
eval' context (TIsZero info x) = liftM (TIsZero info) (eval' context x)

eval' context (TPair info t1 t2) | isVal t2  = liftM2 (TPair info) (eval' context t1) (return t2)
                                 | otherwise = liftM2 (TPair info) (return t1) (eval' context t2)

eval' context (TRetrieve _ (TPair _ t _) 0) = return t
eval' context (TRetrieve _ (TPair _ _ t) 1) = return t
eval' context (TRetrieve info t n) = liftM (\t' -> TRetrieve info t' n) (eval' context t)

eval' context (TRecord _ fields) | all (\(_, v) -> isVal v) fields = Nothing
eval' context (TRecord info fields) = return $ TRecord info $ fmap (pairEval context) fields
                                where pairEval c (k,v) = (k, f context v)
                                      f c t = case eval' c t of
                                                   Just t' -> f c t'
                                                   Nothing -> t

eval' context (TLookup _ (TRecord _ fields) k) = lookup k fields
eval' context (TLet info x t1 t2) | isVal t1  = return $ substitutionTop t1 t2
                                  | otherwise = liftM2 (TLet info x) (eval' context t1) (return t2)

eval' context (TAscribe _ t ty) = return t

eval' context (TCase _ tv@(TAscribe _ (TVariant _ k v) variantType) fields) | isVal tv = do
    t <- lookup k $ (\((Pattern pkey varname), t) -> (pkey, t)) <$> fields
    return $ substitutionTop v t

eval' context (TCase info tv@(TVariant _ k v) fields) | isVal tv = do
    t <- lookup k $ (\((Pattern pkey varname), t) -> (pkey, t)) <$> fields
    return $ substitutionTop v t

eval' context (TCase info t fields) = liftM (\t' -> TCase info t' fields) (eval' context t)

eval' context t1@(TFix _ (TAbs _ _ _ t2)) = return $ substitutionTop t1 t2
eval' context (TFix info t) = liftM (TFix info) (eval' context t)

eval' _ _ = Nothing

typeof :: Context -> Term -> Either String Type
typeof context (TTrue _) = return TyBool
typeof context (TFalse _) = return TyBool
typeof context (TString _ _) = return TyString
typeof context (TUnit _) = return TyUnit
typeof context (TZero _)= return TyNat

typeof context (TSucc _ t) | typeof context t == (Right TyNat) = return TyNat
                           | otherwise = Left $ "argument of succ is not a number (" ++ (show $ typeof context t) ++ ")"

typeof context (TPred _ t) | typeof context t == (Right TyNat) = return TyNat
                           | otherwise = Left $ "argument of pred is not a number (" ++ (show $ typeof context t) ++ ")"

typeof context (TIsZero _ t) | typeof context t == (Right TyNat) = return TyBool
                             | otherwise = Left $ "argument of isZero is not a number " ++ (show $ fromRight $ typeof context t)

typeof context (TIf info t1 t2 t3) | typeof context t1 == Right TyBool =
    let t2' = typeof context t2
        t3' = typeof context t3
    in if t2' == t3'
       then t2'
       else Left $ "arms of conditional have different types " ++ (show info)

typeof context (TIf info _ _ _) = Left $ "guard of conditional not a boolean " ++ (show info)

typeof context v@(TVar info name depth) =
    let ty = typeFromContext context name
    in case ty of
            Just (VarBind ty') -> Right ty'
            Just _ -> Left $ "Wrong kind of binding for variable (" ++ show context ++ " " ++ show v ++ ")"
            Nothing -> Left $ "var type error " ++ show name ++ " " ++ show context ++ " " ++ show depth

typeof context (TApp info t1 t2) =
    let ty1 = typeof context t1
        ty2 = typeof context t2
    in case ty1 of
        Right (TyArrow ty1' ty2') -> if ty2 == (Right ty1')
                                     then Right ty2'
                                     else Left $ "type mismatch in " ++ show info
        Left x -> Left x

typeof context (TAbs info name ty t) =
    let context' = bind name (VarBind ty) context
        ty' = typeof context' t
    in case ty' of
            Right ty'' -> Right $ TyArrow ty ty''
            Left x -> Left x

typeof context (TFloat info x) = Right TyFloat
typeof context (TPair info t1 t2) = liftM2 (TyProduct) (typeof context t1) (typeof context t2)

typeof context (TRetrieve _ (TPair _ t _) 0) = typeof context t
typeof context (TRetrieve _ (TPair _ _ t) 1) = typeof context t
typeof context (TRetrieve _ t@(TVar _ _ _) n) =
    case (typeof context t, n) of
         ((Right (TyProduct t _)), 0) -> Right t
         ((Right (TyProduct _ t)), 1) -> Right t
         (x, _) -> x

typeof context (TRecord _ fields) = if all isRight $ map snd fieldTypes
                                    then Right $ TyRecord validFields
                                    else snd $ head invalidFields
                              where fieldType (k,t) = (k, typeof context t)
                                    fieldTypes = map fieldType fields
                                    validFields = fmap (\(x,y) -> (x, fromRight y)) $ filter (\(x,y) -> isRight y) fieldTypes
                                    invalidFields = filter (\(x,y) -> isLeft y) fieldTypes

typeof context (TVariant _ k t) = liftM (\t' -> TyVariant [(k, t')] ) (typeof context t)

typeof context (TLookup _ (TRecord _ fields) k) = typeof context $ fromJust $ lookup k fields

typeof context (TLookup info v k) = case typeof context v of
                                         Right (TyRecord fields) -> return $ fromJust $ lookup k fields
                                         _ -> error $ show $ TLookup info v k

typeof context (TLet _ x t1 t2) =
    let c' = liftM (\v -> bind x (VarBind v) context) (typeof context t1)
    in case c' of
            Right x -> typeof x t2
            Left x -> Left x

typeof context (TAscribe _ (TVariant _ k t) ty2@(TyVariant fields)) =
    case lookup k fields of
         Just fieldTy -> case typeof context t of
                              Right ty -> if ty == fieldTy then Right ty2 else Left $ "type missmatch in " ++ k ++ " variant"
                              Left x -> Left x
         _ -> Left $ "the annotated type " ++ show ty2 ++ " has't variant " ++ k

typeof context (TAscribe _ t ty) =
    case typeof context t of
         Right ty' | ty' == ty -> return ty'
         Right ty' -> Left $ "the annotated type does't coincide with the real (" ++ (show ty) ++ " and " ++ (show ty') ++ ")"
         x -> x

typeof context (TCase _ v@(TAscribe info (TVariant _ variantField variant) tyVariant) caseFields) = do
    let vfs = variantFields tyVariant
        variantKeys = fst <$> vfs

        caseKeys = (\(Pattern f v) -> f) <$> fst <$> caseFields
        invalidCaseFields = caseKeys \\ variantKeys
        absentCaseFields = variantKeys \\ caseKeys

        pattenWithVariantTypes ((Pattern fname varname), t) = (((Pattern fname varname), t), lookup fname vfs)
        pfs = pattenWithVariantTypes <$> caseFields

        caseFieldsType (((Pattern _ varname), t), Just ty) = typeof (bind varname (VarBind ty) context) t
        caseFieldsTypes = caseFieldsType <$> pfs

        invalidCaseFieldsTypes =  filter isLeft caseFieldsTypes
        validCaseFieldsTypes = nub $ fromRight <$> caseFieldsTypes

        (t, v, ty) = fromJust $ lookup variantField $ (\(((Pattern f v), t), Just ty) -> (f, (t, v, ty))) <$> pfs

    if invalidCaseFields /= []
    then Left $ "Invalid case fields " ++ intercalate ", " invalidCaseFields
    else if absentCaseFields /= []
         then Left $ "Absent case fields " ++ intercalate ", " absentCaseFields
         else if invalidCaseFieldsTypes /= []
              then Left $ "Invalid case fields types " ++ intercalate ", " (fromLeft <$> invalidCaseFieldsTypes)
              else if (length validCaseFieldsTypes) > 1
                   then Left $ "Case fields have different types " ++ intercalate ", " (show <$> validCaseFieldsTypes)
                   else typeof (bind v (VarBind ty) context) t

typeof context (TCase _ v@(TVariant _ variantField variant) caseFields) = do
    tyVariant <- typeof context v

    let vfs = variantFields tyVariant
        variantKeys = fst <$> vfs

        caseKeys = (\(Pattern f v) -> f) <$> fst <$> caseFields
        invalidCaseFields = caseKeys \\ variantKeys
        absentCaseFields = variantKeys \\ caseKeys

        pattenWithVariantTypes ((Pattern fname varname), t) = (((Pattern fname varname), t), lookup fname vfs)
        pfs = pattenWithVariantTypes <$> caseFields

        caseFieldsType (((Pattern _ varname), t), Just ty) = typeof (bind varname (VarBind ty) context) t
        caseFieldsTypes = caseFieldsType <$> pfs

        invalidCaseFieldsTypes =  filter isLeft caseFieldsTypes
        validCaseFieldsTypes = nub $ fromRight <$> caseFieldsTypes

        (t, v, ty) = fromJust $ lookup variantField $ (\(((Pattern f v), t), Just ty) -> (f, (t, v, ty))) <$> pfs

    if invalidCaseFields /= []
    then Left $ "Invalid case fields " ++ intercalate ", " invalidCaseFields
    else if absentCaseFields /= []
         then Left $ "Absent case fields " ++ intercalate ", " absentCaseFields
         else if invalidCaseFieldsTypes /= []
              then Left $ "Invalid case fields types " ++ intercalate ", " (fromLeft <$> invalidCaseFieldsTypes)
              else if (length validCaseFieldsTypes) > 1
                   then Left $ "Case fields have different types " ++ intercalate ", " (show <$> validCaseFieldsTypes)
                   else typeof (bind v (VarBind ty) context) t

typeof context (TFix _ t) = do
    ty <- typeof context t
    case ty of
        (TyArrow t1 t2) | t1 == t2 -> return t2
        (TyArrow _ _) -> Left $ "result of body not compatible with domain"
        _ -> Left $ "arrow type expected"

typeof context t = error $ show t

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TFloat _ _) = True
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal x | isNumerical x = True
isVal (TRecord _ ts) = all (\(_,y) -> isVal y) ts
isVal (TAscribe _ t _) = isVal t
isVal (TVariant _ _ t) = isVal t
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk c (TTrue info) = TTrue info
                  walk c (TFalse info) = TFalse info
                  walk c (TString info s) = TString info s
                  walk c (TUnit info) = TUnit info
                  walk c (TZero info) = TZero info
                  walk c (TIsZero info t) = TIsZero info (walk c t)
                  walk c (TPred info t) = TPred info (walk c t)
                  walk c (TSucc info t) = TSucc info (walk c t)
                  walk c (TFloat info t) = TFloat info t
                  walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                  walk c (TRetrieve info t n) = TRetrieve info (walk c t) n
                  walk c (TRecord info fields) = TRecord info $ fmap (\(k,v) -> (k, walk c v)) fields
                  walk c (TVariant info k v) = TVariant info k (walk c v)
                  walk c (TLookup info r k) = TLookup info (walk c r) k
                  walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)
                  walk c (TAscribe info t ty) = TAscribe info (walk c t) ty
                  walk c (TCase info t fields) = TCase info (walk c t) $ walkField <$> fields
                                           where walkField (p, x) = (p, walk (c + 1) x)

                  walk c (TFix info t) = TFix info (walk c t)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = tmmap onvar c t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar c info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c info name depth | name == j + c = shift c s
                     onvar c info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
