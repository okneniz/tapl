module Language.TAPL.RcdSubBot.Parser (parse) where

import Language.TAPL.RcdSubBot.Types
import Language.TAPL.RcdSubBot.Context
import Language.TAPL.RcdSubBot.Lexer
import Language.TAPL.Common.Helpers (ucid, padded, withState)
import Language.TAPL.Common.Context (findVarName)

import Data.Functor (($>))
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser reconParser []

reconParser :: Parsec String LCNames ([Command], LCNames)
reconParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command =  padded (bindCommand <|> evalCommand)

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* reservedOp "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = apply <|> notApply <|> parens term

apply :: LCParser
apply = try $ chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = value
       <|> (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> (parens notApply)

value :: LCParser
value = record <?> "record"

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    name <- reserved "lambda" *> identifier
    ty <- colon *> typeAnnotation <* dot
    withState (addVar name ty) $ TAbs pos name ty <$> term

variable :: LCParser
variable = optionalProjection identifier $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context " <> " " <> (show pos)

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    try (dotRef key t) <|> return t
    where dotRef k t = do
            pos <- dot *> getPosition
            i <- k
            try (dotRef k (TProj pos t i)) <|> return (TProj pos t i)

record :: LCParser
record = try $ braces $ TRecord <$> getPosition
                                <*> (Map.fromList <$> (keyValue (reservedOp "=") notApply) `sepBy` comma)

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") $> TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = topAnnotation <|> botAnnotation <|> recordAnnotation

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name $> ty

recordAnnotation :: LCTypeParser
recordAnnotation = try $ braces $ TyRecord <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

