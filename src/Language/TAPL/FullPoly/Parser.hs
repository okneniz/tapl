module Language.TAPL.FullPoly.Parser (parse) where

import Language.TAPL.FullPoly.Types
import Language.TAPL.FullPoly.Context
import Language.TAPL.FullPoly.Lexer
import Language.TAPL.Common.Helpers (ucid)
import Language.TAPL.Common.Context (findVarName)

import Prelude hiding (abs, succ, pred)
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser fullPolyParser []

fullPolyParser :: Parsec String LCNames ([Command], LCNames)
fullPolyParser = do
    commands <- command `sepEndBy` semi <* eof
    names <- getState
    return $ (commands, names)

command :: Parsec String LCNames Command
command = (optional spaces) >> ((try bindCommand)
                            <|> (try someBindCommand)
                            <|> evalCommand)

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces
    reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

someBindCommand :: LCCommandParser
someBindCommand = braces $ do
    pos <- getPosition
    u <- ucid <* spaces
    modifyState $ addName u
    l <- comma *> identifier <* reserved "="
    modifyState $ addName l
    t <- notApply
    return $ SomeBind pos u l t

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = try typeApply
   <|> try typeAbstraction
   <|> try termApply
   <|> try notApply
   <|> try unpack
   <|> parens term

typeApply :: LCParser
typeApply = TTApp <$> getPosition
                  <*> (typeAbstraction <* spaces)
                  <*> (brackets typeAnnotation)

termApply :: LCParser
termApply = chainl1 (notApply <|> parens termApply) $ TApp <$> (optional spaces *> getPosition)

notApply :: LCParser
notApply = optionalAscribed
         $ projection (try keyword <|> integer)
         $ try value <|> try stdFuncs
                     <|> try (condition <?> "condition")
                     <|> try (pack <?> "pack")
                     <|> try (unpack <?> "unpack")
                     <|> try (letT <?> "let")
                     <|> try (fix <?> "fix")
                     <|> try (variable <?> "variable")
                     <|> try (parens notApply)

notTypeBind :: LCParser
notTypeBind = try termApply
          <|> try notApply
          <|> try (parens notTypeBind)

stdFuncs :: LCParser
stdFuncs = (timesFloat <?> "timesfloat") <|> (isZero <?> "zero?")

value :: LCParser
value = nat
    <|> (abstraction <?> "abstraction")
    <|> (boolean <?> "boolean")
    <|> (unit <?> "unit")
    <|> (stringT <?> "string")
    <|> (float <?> "float")
    <|> (integer <?> "integer")
    <|> (record <?> "record")
    <|> (parens value)

nat :: LCParser
nat = (succ <?> "succ")
  <|> (pred <?> "pred")
  <|> (zero <?> "zero")

pack :: LCParser
pack = do
    pos <- getPosition
    (ty1, t) <- braces $ (,) <$> (reservedOp "*" *> typeAnnotation) <*> (comma *> notTypeBind)
    ty2 <- reserved "as" *> typeAnnotation
    return $ TPack pos ty1 t ty2

unpack :: LCParser
unpack = do
    pos <- reserved "let" *> getPosition
    (x,y) <- braces $ (,) <$> ucid <*> (comma *> identifier)
    t1 <- reservedOp "=" *> notTypeBind
    names <- getState
    modifyState $ addName x
    modifyState $ addName y
    t2 <- reserved "in" *> notTypeBind
    setState names
    return $ TUnpack pos x y t1 t2

typeAbstraction :: LCParser
typeAbstraction = parens $ TTAbs <$> getPosition
                                 <*> (reserved "lambda" *> ucid <* dot)
                                 <*> notTypeBind

abstraction :: LCParser
abstraction = do
    pos <- getPosition <* reserved "lambda"
    name <- identifier
    ty <- termType <* dot <* optional spaces
    names <- getState
    modifyState $ addVar name ty
    t <- notTypeBind
    setState names
    return $ TAbs pos name ty t

variable :: LCParser
variable = do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

stringT :: LCParser
stringT = TString <$> getPosition <*> try stringLiteral

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> term

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

float :: LCParser
float = TFloat <$> getPosition <*> floatNum

integer :: LCParser
integer = TInt <$> getPosition <*> natural

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name >> (t <$> getPosition)

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> notTypeBind)
                <*> (reserved "then" *> notTypeBind)
                <*> (reserved "else" *> notTypeBind)

projection :: LCParser -> LCParser -> LCParser
projection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'

dotRef :: LCParser -> Term -> LCParser
dotRef key t = do
    pos <- dot *> getPosition
    i <- key
    t' <- (try $ dotRef key (TProj pos t i)) <|> (return $ TProj pos t i)
    return t'

optionalAscribed :: LCParser -> LCParser
optionalAscribed e = do
    t <- e
    t' <- (try $ f t) <|> (return t)
    return t'
  where f t = do
          spaces
          reserved "as"
          optional spaces
          ty <- typeAnnotation
          pos <- getPosition
          return $ TAscribe pos t ty

record :: LCParser
record = braces $ TRecord <$> getPosition
                          <*> (Map.fromList <$> (keyValue (reservedOp "=") notTypeBind) `sepBy` comma)

keyword :: LCParser
keyword = TKeyword <$> getPosition <*> identifier

letT :: LCParser
letT = do
    p <- getPosition <* reserved "let"
    name <- identifier <* reservedOp "="
    t1 <- notTypeBind <* reserved "in"
    names <- getState
    modifyState $ addName name
    t2 <- notTypeBind
    setState names -- TODO ?
    return $ TLet p name t1 t2

timesFloat :: LCParser
timesFloat = TTimesFloat <$> (reserved "timesfloat" *> getPosition)
                         <*> notApply
                         <*> (spaces *> notApply)

fix :: LCParser
fix = TFix <$> (reserved "fix" *> getPosition) <*> notTypeBind

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    optional spaces
    reservedOp "->"
    optional spaces
    return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = stringAnnotation
                 <|> unitAnnotation
                 <|> booleanAnnotation
                 <|> natAnnotation
                 <|> floatAnnotation
                 <|> try existentialType
                 <|> try recordAnnotation
                 <|> universalType
                 <|> typeVarOrID

universalType :: LCTypeParser
universalType = do
    x <- reserved "All" *> ucid
    names <- getState
    modifyState $ addName x
    ty <- dot *> typeAnnotation
    setState names
    return $ TyAll x ty

existentialType :: LCTypeParser
existentialType = braces $ do
    x <- reserved "Some" *> ucid
    names <- getState
    modifyState $ addName x
    ty <- comma *> typeAnnotation
    setState names
    return $ TySome x ty

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

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

recordAnnotation :: LCTypeParser
recordAnnotation = braces $ TyRecord <$> Map.fromList
                                     <$> (keyValue colon typeAnnotation) `sepBy` comma

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    names <- getState
    return $ case findVarName names name of
                  Just varName -> TyVar varName (length names)
                  Nothing -> TyID name
