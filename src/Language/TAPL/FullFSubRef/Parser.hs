module Language.TAPL.FullFSubRef.Parser (parse) where

import Language.TAPL.FullFSubRef.Types
import Language.TAPL.FullFSubRef.Context
import Language.TAPL.FullFSubRef.Lexer
import Language.TAPL.Common.Helpers (ucid, padded)
import Language.TAPL.Common.Context (findVarName)

import Data.Functor (($>))
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser fullFSubRefParser []

fullFSubRefParser :: Parsec String LCNames ([Command], LCNames)
fullFSubRefParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = padded $ bindCommand <|> evalCommand

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces <* reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

--someBindCommand :: LCCommandParser
--someBindCommand = try $ braces $ do
--    pos <- getPosition
--    u <- ucid <* spaces
--    modifyState $ addName u
--    l <- comma *> identifier <* reserved "="
--    modifyState $ addName l
--    t <- notApply
--    return $ SomeBind pos u l t

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = typeApply
   <|> termApply
   <|> notApply
   <|> (parens term)

termApply :: LCParser
termApply = chainl1 (notApply <|> parens termApply) $ TApp <$> (optional spaces *> getPosition)

typeApply :: LCParser
typeApply = try $ TTApp <$> getPosition
                        <*> (typeAbstraction <|> try (parens typeAbstraction) <|> try variable)
                        <*> brackets typeAnnotation

notApply :: LCParser
notApply = value
       <|> stdFuncs
       <|> construction
       <|> (assignT <?> "assignment")
       <|> (variable <?> "variable")
       <|> (parens notApply)

notTypeBind :: LCParser
notTypeBind = termApply
          <|> notApply
          <|> (optionalProjection identifier (parens notTypeBind))

assignT :: LCParser
assignT = chainl1 (notAssign <|> parens notAssign) $ (padded $ reservedOp ":=") >> TAssign <$> getPosition

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
           <|> (letT <?> "let")
           <|> (caseT <?> "case")

value :: LCParser
value = optionalAscribed $ nat <|> (boolean <?> "boolean")
                               <|> (unit <?> "unit")
                               <|> (errorT <?> "error")
                               <|> (stringT <?> "string")
                               <|> (float <?> "float")
                               <|> (variant <?> "variant")
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
derefT = TDeref <$> (reservedOp "!" >> getPosition) <*> notTypeBind

nat :: LCParser
nat = (zero <?> "zero") <|> (succ <?> "succ") <|> (pred <?> "pred")
  where zero = constant "zero" TZero
        succ = fun "succ" TSucc
        pred = fun "pred" TPred

typeAbstraction :: LCParser
typeAbstraction = try $ do
    p <- getPosition
    x <- reserved "lambda" *> ucid
    ty <- optionalType <* dot
    names <- getState
    modifyState $ addName x
    t <- notTypeBind
    setState names
    return $ TTAbs p x ty t

abstraction :: LCParser
abstraction = try $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- termType <* dot
    names <- getState
    modifyState $ addVar name ty
    t <- notTypeBind
    setState names
    return $ TAbs pos name ty t

variable :: LCParser
variable = optionalProjection identifier $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> unexpected $ "variable " ++ show name ++ " hasn't been bound in context " ++ " " ++ (show names)

variant :: LCParser
variant = try $ do
  pos <- getPosition
  (key, t) <- angles $ keyValue (reservedOp "=") notApply
  ty <- reserved "as" *> variantAnnotation
  return $ TTag pos key t ty

record :: LCParser
record = try $ braces $ TRecord <$> getPosition
                                <*> (Map.fromList <$> (keyValue (reservedOp "=") notApply) `sepBy` comma)

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (padded identifier <* devider) <*> padded val

stringT :: LCParser
stringT = TString <$> getPosition <*> try stringLiteral

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

float :: LCParser
float = TFloat <$> getPosition <*> try floatNum

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name >> (t <$> getPosition)

unit :: LCParser
unit = constant "unit" TUnit

errorT :: LCParser
errorT = constant "error" TError

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> notTypeBind)
                <*> (reserved "then" *> notTypeBind)
                <*> (reserved "else" *> notTypeBind)

caseT :: LCParser
caseT = TCase <$> getPosition
              <*> (reserved "case" *> padded term <* reserved "of")
              <*> (Map.fromList <$> branch `sepBy` (reserved "|"))
  where branch = do
          (caseName, varName) <- angles $ keyValue (reservedOp "=") identifier
          reservedOp "->"
          context <- getState
          modifyState $ addName varName
          t2 <- term
          setState context
          return (caseName, (varName, t2))

letT :: LCParser
letT = do
    p <- getPosition <* reserved "let"
    name <- identifier <* reservedOp "="
    t1 <- (typeApply <|> notTypeBind) <* reserved "in"
    names <- getState
    modifyState $ addName name
    t2 <- notTypeBind
    setState names
    return $ TLet p name t1 t2

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
    where dotRef key t = do
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

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> (notApply <|> parens termApply)

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ (padded $ reservedOp "->") $> TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = topAnnotation
                 <|> botAnnotation
                 <|> stringAnnotation
                 <|> unitAnnotation
                 <|> booleanAnnotation
                 <|> natAnnotation
                 <|> floatAnnotation
                 <|> refAnnotation
                 <|> sourceAnnotation
                 <|> sinkAnnotation
                 <|> try recordAnnotation
                 <|> try variantAnnotation
                 <|> universalType
                 <|> typeVarOrID

universalType :: LCTypeParser
universalType = do
    x <- reserved "All" *> ucid
    ty1 <- optionalType
    names <- getState
    modifyState $ addName x
    ty2 <- dot *> typeAnnotation
    setState names
    return $ TyAll x ty1 ty2

optionalType :: LCTypeParser
optionalType = try (reservedOp "<:" *> typeAnnotation) <|> return TyTop

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

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

sourceAnnotation :: LCTypeParser
sourceAnnotation = TySource <$> (reserved "Source" *> typeAnnotation)

sinkAnnotation :: LCTypeParser
sinkAnnotation = TySink <$> (reserved "Sink" *> typeAnnotation)

recordAnnotation :: LCTypeParser
recordAnnotation = braces $ TyRecord <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

variantAnnotation :: LCTypeParser
variantAnnotation = angles $ TyVariant <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    names <- getState
    return $ case findVarName names name of
                  Just varName -> TyVar varName (length names)
                  Nothing -> TyID name
