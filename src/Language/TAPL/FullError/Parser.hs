module Language.TAPL.FullError.Parser (parse) where

import Language.TAPL.FullError.Types
import Language.TAPL.FullError.Context
import Language.TAPL.FullError.Lexer
import Language.TAPL.Common.Helpers (ucid)
import Language.TAPL.Common.Context (findVarName)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser reconParser []

reconParser :: Parsec String LCNames ([Command], LCNames)
reconParser = do
    commands <- command `sepEndBy` semi
    eof
    names <- getState
    return $ (commands, names)

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
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp pos

notApply :: LCParser
notApply = try (boolean <?> "boolean")
       <|> try (error' <?> "error")
       <|> try (condition <?> "condition")
       <|> try (abstraction <?> "abstraction")
       <|> try (try' <?> "try")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)
       <|> try (parens apply)

notTypeBind :: LCParser
notTypeBind = try apply
          <|> try notApply
          <|> try (parens notTypeBind)

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    name <- identifier
    ty <- termType
    dot
    optional spaces
    names <- getState
    modifyState $ addVar name ty
    t <- notTypeBind
    setState names
    return $ TAbs pos name ty t

try' :: LCParser
try' = do
    reserved "try"
    pos <- getPosition
    t1 <- term
    reserved "with"
    t2 <- term
    return $ TTry pos t1 t2

variable :: LCParser
variable = do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context " <> " " <> (show pos)

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

error' :: LCParser
error' = constant "error" TError

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name >> (t <$> getPosition)

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if"   *> notTypeBind)
                <*> (reserved "then" *> notTypeBind)
                <*> (reserved "else" *> notTypeBind)

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation
             <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    optional spaces
    reservedOp "->"
    optional spaces
    return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation
                 <|> topAnnotation
                 <|> botAnnotation

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty
