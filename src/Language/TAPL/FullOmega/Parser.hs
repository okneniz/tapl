module Language.TAPL.FullOmega.Parser (parse) where

import Language.TAPL.FullOmega.Types
import Language.TAPL.FullOmega.Context
import Language.TAPL.FullOmega.Lexer
import Language.TAPL.Common.Helpers (ucid, padded, withState)
import Language.TAPL.Common.Context (findVarName)

import Data.Functor (($>))

import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type
type LCKindParser = Parsec String LCNames Kind

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser fullOmegaParser []

fullOmegaParser :: Parsec String LCNames ([Command], LCNames)
fullOmegaParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = padded $ bindCommand <|> evalCommand

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces <* reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty Nothing

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = termApply
   <|> typeApply
   <|> notApply
   <|> (parens term)

termApply :: LCParser
termApply = try $ chainl1 p $ TApp <$> (optional spaces *> getPosition)
    where p = try typeApply <|> try notApply <|> try (parens typeApply) <|> parens termApply

typeApply :: LCParser
typeApply = try $ do
    p <- getPosition
    t <- try typeAbstraction <|> variable
    ty <- z
    try (f $ TTApp p t ty) <|> return (TTApp p t ty)
    where z = bracketType <|> typeAbstractionAnnotation <|> typeVarOrID
          f t1 = do
            t2 <- TTApp <$> getPosition <*> (return t1) <*> z
            try (f t2) <|> (return t2)

notApply :: LCParser
notApply = value
       <|> stdFuncs
       <|> construction
       <|> (assignT <?> "assignment")
       <|> (variable <?> "variable")
       <|> (parens notApply)

assignT :: LCParser
assignT = chainl1 (notAssign <|> parens notAssign) $ padded (reservedOp ":=") >> TAssign <$> getPosition

notAssign :: LCParser
notAssign = value
        <|> stdFuncs
        <|> construction
        <|> (variable <?> "variable")
        <|> (parens termApply)
        <|> (parens notAssign)

stdFuncs :: LCParser
stdFuncs = (timesFloat <?> "timesfloat")
       <|> (isZero <?> "zero?")
       <|> (fix <?> "fix")
       <|> (ref <?> "ref")
       <|> (derefT <?> "deref")

construction :: LCParser
construction = (condition <?> "condition")
           <|> (pack <?> "pack")
           <|> (unpack <?> "unpack")
           <|> (letT <?> "let")

value :: LCParser
value = optionalAscribed $ (boolean <?> "boolean")
                       <|> (float <?> "float")
                       <|> (nat <?> "nat")
                       <|> (unit <?> "unit")
                       <|> (stringT <?> "string")
                       <|> ((optionalProjection identifier record) <?> "record")
                       <|> (typeAbstraction <?> "type abstraction")
                       <|> (abstraction <?> "abstraction")

isZero :: LCParser
isZero = fun "zero?" TIsZero

ref :: LCParser
ref = fun "ref" TRef

fix :: LCParser
fix = fun "fix" TFix

timesFloat :: LCParser
timesFloat = TTimesFloat <$> (reserved "timesfloat" *> getPosition) <*> notApply <*> (spaces *> notApply)

derefT :: LCParser
derefT = TDeref <$> (reservedOp "!" >> getPosition) <*> term

nat :: LCParser
nat = succ <|> pred <|> zero <|> integer
    where succ = fun "succ" TSucc
          pred = fun "pred" TPred
          zero = constant "zero" TZero
          integer = do
            p <- getPosition
            i <- try natural
            toNat p i (TZero p)
          toNat _ i _ | i < 0 = unexpected "unexpected negative number"
          toNat _ 0 t = return t
          toNat p i t = toNat p (i - 1) (TSucc p t)

typeAbstraction :: LCParser
typeAbstraction = try $ optionalParens $ do
    p <- getPosition
    x <- reserved "lambda" *> ucid
    k <- optionalKind <* dot
    withState (addName x) $ TTAbs p x k <$> term

pack :: LCParser
pack = try $ do
    pos <- getPosition
    (ty1, t) <- braces $ (,) <$> (reservedOp "*" *> typeAnnotation) <*> (comma *> term)
    ty2 <- reserved "as" *> typeAnnotation
    return $ TPack pos ty1 t ty2

unpack :: LCParser
unpack = try $ do
    pos <- reserved "let" *> getPosition
    (x,y) <- braces $ (,) <$> ucid <*> (comma *> identifier)
    t1 <- reservedOp "=" *> term <* reserved "in"
    withState (addName x) $ do
      withState (addName y) $ do
        TUnpack pos x y t1 <$> term

abstraction :: LCParser
abstraction = try $ optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- colon *> typeAnnotation <* dot
    withState (addVar name ty) $ TAbs pos name ty <$> term

variable :: LCParser
variable = try $ optionalProjection identifier $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> unexpected $ "variable " <> show name <> " hasn't been bound in context " <> (show names)

record :: LCParser
record = try $ braces $ TRecord <$> getPosition
                                <*> (Map.fromList <$> (keyValue (reservedOp "=") notApply) `sepBy` comma)

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (padded identifier <* devider) <*> padded val

stringT :: LCParser
stringT = TString <$> getPosition <*> try stringLiteral

float :: LCParser
float = TFloat <$> getPosition <*> try floatNum

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name >> (t <$> getPosition)

unit :: LCParser
unit = constant "unit" TUnit

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

letT :: LCParser
letT = do
    p <- getPosition <* reserved "let"
    name <- identifier <* reservedOp "="
    t1 <- term <* reserved "in"
    withState (addName name) $ TLet p name t1 <$> term

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    try (dotRef key t) <|> return t
    where dotRef key t = do
            pos <- dot *> getPosition
            i <- key
            try (dotRef key (TProj pos t i)) <|> return (TProj pos t i)

optionalAscribed :: LCParser -> LCParser
optionalAscribed e = do
    t <- e
    try (f t) <|> return t
  where f t = do
          spaces
          reserved "as"
          optional spaces
          ty <- typeAnnotation
          pos <- getPosition
          return $ TAscribe pos t ty

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> (notApply <|> parens termApply)

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = try $ chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    (padded $ reservedOp "->")
    return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = stringAnnotation
                 <|> unitAnnotation
                 <|> booleanAnnotation
                 <|> natAnnotation
                 <|> floatAnnotation
                 <|> refAnnotation
                 <|> existentialType
                 <|> recordAnnotation
                 <|> universalType
                 <|> typeAbstractionAnnotation
                 <|> typeApplyAnnotation
                 <|> typeVarOrID

bracketType :: LCTypeParser
bracketType = brackets $ arrowAnnotation
                     <|> stringAnnotation
                     <|> unitAnnotation
                     <|> booleanAnnotation
                     <|> natAnnotation
                     <|> floatAnnotation
                     <|> refAnnotation
                     <|> existentialType
                     <|> recordAnnotation
                     <|> universalType
                     <|> typeVarOrID

universalType :: LCTypeParser
universalType = try $ do
    x <- reserved "All" *> ucid
    k <- optionalKind <* dot
    withState (addName x) $ TyAll x k <$> typeAnnotation

existentialType :: LCTypeParser
existentialType = try $ braces $ do
    x <- reserved "Some" *> ucid
    k <- optionalKind <* comma
    withState (addName x) $ TySome x k <$> typeAnnotation

typeAbstractionAnnotation :: LCTypeParser
typeAbstractionAnnotation = try $ optionalParens $ do
    x <- reserved "lambda" *> ucid
    k <- optionalKind <* dot
    withState (addName x) $ TyAbs x k <$> typeAnnotation

typeApplyAnnotation :: LCTypeParser
typeApplyAnnotation =
    try $ chainl1 (typeVarOrID <|> bracketType <|> (parens typeApplyAnnotation)) $ do
        optional spaces
        return TyApp

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name $> ty

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

stringAnnotation :: LCTypeParser
stringAnnotation = primitiveType "String" TyString

unitAnnotation :: LCTypeParser
unitAnnotation = primitiveType "Unit" TyUnit

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

floatAnnotation :: LCTypeParser
floatAnnotation = primitiveType "Float" TyFloat

refAnnotation :: LCTypeParser
refAnnotation = TyRef <$> (reserved "Ref" *> typeAnnotation)

recordAnnotation :: LCTypeParser
recordAnnotation =
    try $ braces $ TyRecord <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

typeVarOrID:: LCTypeParser
typeVarOrID = try $ do
    name <- ucid
    names <- getState
    return $ case findVarName names name of
                  Just varName -> TyVar varName (length names)
                  Nothing -> TyID name

optionalKind :: LCKindParser
optionalKind = (reservedOp "::" >> kindAnnotation) <|> return Star

kindAnnotation :: LCKindParser
kindAnnotation = arrowKind <|> startKind

startKind :: LCKindParser
startKind = reservedOp "*" $> Star

arrowKind :: LCKindParser
arrowKind = chainr1 (startKind <|> parens kindAnnotation) $ padded (reservedOp "->") $> Arrow

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> try f
