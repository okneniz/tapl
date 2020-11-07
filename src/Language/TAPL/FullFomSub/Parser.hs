module Language.TAPL.FullFomSub.Parser (parse) where

import Language.TAPL.FullFomSub.Types
import Language.TAPL.FullFomSub.Context
import Language.TAPL.FullFomSub.Lexer
import Language.TAPL.Common.Helpers (ucid, padded)
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
    (try $ f $ TTApp p t ty) <|> (return $ TTApp p t ty)
    where z = bracketType <|> typeAbstractionAnnotation <|> typeVarOrID
          f t1 = do
            t2 <- TTApp <$> getPosition <*> (return t1) <*> z
            (try $ f t2) <|> (return t2)

notApply :: LCParser
notApply = stdFuncs
       <|> construction
       <|> value
       <|> (variable <?> "variable")
       <|> (parens notApply)

notTypeBind :: LCParser
notTypeBind = termApply
          <|> notApply
          <|> (optionalProjection identifier (parens notTypeBind))

stdFuncs :: LCParser
stdFuncs = (timesFloat <?> "timesfloat")
       <|> (isZero <?> "zero?")
       <|> (fix <?> "fix")
       <|> try (parens stdFuncs)

construction :: LCParser
construction = (condition <?> "condition")
           <|> (pack <?> "pack")
           <|> (unpack <?> "unpack")
           <|> (letT <?> "let")

value :: LCParser
value = optionalAscribed $ nat <|> (boolean <?> "boolean")
                               <|> (unit <?> "unit")
                               <|> (stringT <?> "string")
                               <|> (float <?> "float")
                               <|> (boolean <?> "boolean")
                               <|> ((optionalProjection identifier record) <?> "record")
                               <|> (typeAbstraction <?> "type abstraction")
                               <|> (abstraction <?> "abstraction")
                               <|> (parens value)

isZero :: LCParser
isZero = fun "zero?" TIsZero

fix :: LCParser
fix = fun "fix" TFix

timesFloat :: LCParser
timesFloat = TTimesFloat <$> (reserved "timesfloat" *> getPosition) <*> notApply <*> (spaces *> notApply)

nat :: LCParser
nat = (zero <?> "zero") <|> (succ <?> "succ") <|> (pred <?> "pred")
  where zero = constant "zero" TZero
        succ = fun "succ" TSucc
        pred = fun "pred" TPred

typeAbstraction :: LCParser
typeAbstraction = try $ optionalParens $ do
    p <- getPosition
    x <- reserved "lambda" *> ucid
    k <- optionalType <* dot
    names <- getState
    modifyState $ addName x
    t <- notTypeBind
    setState names
    return $ TTAbs p x k t

pack :: LCParser
pack = try $ do
    pos <- getPosition
    (ty1, t) <- braces $ (,) <$> (reservedOp "*" *> typeAnnotation) <*> (comma *> notTypeBind)
    ty2 <- reserved "as" *> typeAnnotation
    return $ TPack pos ty1 t ty2

unpack :: LCParser
unpack = try $ do
    pos <- reserved "let" *> getPosition
    (x,y) <- braces $ (,) <$> ucid <*> (comma *> identifier)
    t1 <- reservedOp "=" *> notTypeBind
    names <- getState
    modifyState $ addName x
    modifyState $ addName y
    t2 <- reserved "in" *> notTypeBind
    setState names
    return $ TUnpack pos x y t1 t2

abstraction :: LCParser
abstraction = try $ optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- colon >> typeAnnotation
    names <- getState
    modifyState $ addVar name ty
    t <- dot *> term
    setState names
    return $ TAbs pos name ty t

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
                                <*> (Map.fromList <$> (keyValue (reservedOp "=") term) `sepBy` comma)

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

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> notTypeBind)
                <*> (reserved "then" *> notTypeBind)
                <*> (reserved "else" *> notTypeBind)

letT :: LCParser
letT = do
    p <- getPosition <* reserved "let"
    name <- identifier <* reservedOp "="
    t1 <- term <* reserved "in"
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
    where dotRef k t = do
            pos <- dot *> getPosition
            i <- key
            t' <- (try $ dotRef k (TProj pos t i)) <|> (return $ TProj pos t i)
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

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

optionalType :: LCTypeParser
optionalType = try top <|> try kind <|> return TyTop
    where top = reservedOp "<:" *> typeAnnotation
          kind = reservedOp "::" *> (makeTop <$> kindAnnotation)

arrowAnnotation :: LCTypeParser
arrowAnnotation = try $ chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    (padded $ reservedOp "->")
    return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = topAnnotation
                 <|> stringAnnotation
                 <|> unitAnnotation
                 <|> booleanAnnotation
                 <|> natAnnotation
                 <|> floatAnnotation
                 <|> existentialType
                 <|> recordAnnotation
                 <|> universalType
                 <|> typeAbstractionAnnotation
                 <|> typeApplyAnnotation
                 <|> typeVarOrID

bracketType :: LCTypeParser
bracketType = brackets $ arrowAnnotation
                     <|> topAnnotation
                     <|> stringAnnotation
                     <|> unitAnnotation
                     <|> booleanAnnotation
                     <|> natAnnotation
                     <|> floatAnnotation
                     <|> existentialType
                     <|> recordAnnotation
                     <|> universalType
                     <|> typeVarOrID

universalType :: LCTypeParser
universalType = try $ do
    x <- reserved "All" *> ucid
    ty1 <- optionalType
    names <- getState
    modifyState $ addName x
    ty2 <- dot *> typeAnnotation
    setState names
    return $ TyAll x ty1 ty2

existentialType :: LCTypeParser
existentialType = try $ braces $ do
    x <- reserved "Some" *> ucid
    ty1 <- optionalType
    names <- getState
    modifyState $ addName x
    ty2 <- comma *> typeAnnotation
    setState names
    return $ TySome x ty1 ty2

typeAbstractionAnnotation :: LCTypeParser
typeAbstractionAnnotation = try $ optionalParens $ do
    x <- reserved "lambda" *> ucid
    k <- optionalKind
    names <- getState
    modifyState $ addName x
    ty <- dot *> typeAnnotation
    setState names
    return $ TyAbs x k ty

typeApplyAnnotation :: LCTypeParser
typeApplyAnnotation =
    try $ chainl1 (typeVarOrID <|> bracketType <|> (parens typeApplyAnnotation)) $ do
        optional spaces
        return TyApp

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

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
startKind = reservedOp "*" >> return Star

arrowKind :: LCKindParser
arrowKind = chainr1 (startKind <|> parens kindAnnotation) $ (padded $ reservedOp "->") $> Arrow

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> try f
