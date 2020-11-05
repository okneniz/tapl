module Language.TAPL.FullSub.Parser (parse) where

import Language.TAPL.FullSub.Types
import Language.TAPL.FullSub.Context
import Language.TAPL.FullSub.Lexer
import Language.TAPL.Common.Helpers (ucid, padded)

import Prelude hiding (abs, succ, pred)
import qualified Data.Map.Lazy as Map
import Data.List (findIndex)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser fullSubParser []

fullSubParser :: Parsec String LCNames ([Command], LCNames)
fullSubParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command =  (try bindCommand) <|> (try evalCommand)

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces
    reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = try $ Eval <$> term `sepEndBy` semi

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = try value
       <|> try (timesFloat <?> "timesfloat")
       <|> try (condition <?> "condition")
       <|> try (letT <?> "let")
       <|> try (fix <?> "fix")
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)

notTypeBind :: LCParser
notTypeBind = try apply
          <|> try notApply
          <|> try (parens notTypeBind)

value :: LCParser
value = optionalAscribed $ (boolean <?> "boolean")
                       <|> (stringT <?> "string")
                       <|> (succ <?> "succ")
                       <|> (pred <?> "pred")
                       <|> (isZero <?> "zero?")
                       <|> (zero <?> "zero")
                       <|> (unit <?> "unit")
                       <|> try (float <?> "float")
                       <|> try (record <?> "record")

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    dot *> optional spaces
    context <- getState
    modifyState $ addVar varName varType
    t <- notTypeBind
    setState context
    return $ TAbs p varName varType t

variable :: LCParser
variable = optionalAscribed $ optionalProjection identifier $ do
    name <- identifier
    names <- getState
    p <- getPosition
    case findIndex ((== name) . fst) names of
         Just n -> return $ TVar p n (length names)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

stringT :: LCParser
stringT = TString <$> getPosition <*> try stringLiteral

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> notTypeBind

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

float :: LCParser
float = TFloat <$> getPosition <*> floatNum

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name *> (t <$> getPosition)

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> notTypeBind)
                <*> (reserved "then" *> notTypeBind)
                <*> (reserved "else" *> notTypeBind)

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
    where dotRef key t = do
            _ <- dot
            pos <- getPosition
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
record = optionalProjection identifier $ braces $ do
    p <- getPosition
    ts <- keyValue (reservedOp "=") notTypeBind `sepBy` comma
    return $ TRecord p $ Map.fromList ts

letT :: LCParser
letT = do
    reserved "let"
    p <- getPosition
    name <- identifier
    reservedOp "="
    t1 <- notTypeBind
    optional spaces
    reserved "in"
    optional spaces
    modifyState $ addName name
    t2 <- notTypeBind
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
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") *> return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = topAnnotation
                 <|> try booleanAnnotation
                 <|> try recordAnnotation
                 <|> try stringAnnotation
                 <|> try unitAnnotation
                 <|> try natAnnotation
                 <|> try floatAnnotation
                 <|> try typeVarOrID

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

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
recordAnnotation = braces $ TyRecord <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    names <- getState
    return $ case findIndex ((== name) . fst) names of
                  Just x -> TyVar x (length names)
                  Nothing -> TyID name
