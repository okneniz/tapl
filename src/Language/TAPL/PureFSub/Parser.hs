module Language.TAPL.PureFSub.Parser (parse) where

import Language.TAPL.PureFSub.Types
import Language.TAPL.PureFSub.Context
import Language.TAPL.PureFSub.Lexer
import Language.TAPL.Common.Helpers (ucid, padded, withState)
import Language.TAPL.Common.Context (findVarName)

import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.Functor (($>))

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser pureFSubParser []

pureFSubParser :: Parsec String LCNames ([Command], LCNames)
pureFSubParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = padded (bindCommand <|> evalCommand)

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces
    reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = try typeApply
   <|> try typeAbstraction
   <|> try termApply
   <|> try notApply
   <|> try (parens term)

typeApply :: LCParser
typeApply = TTApp <$> getPosition <*> (typeAbstraction <* spaces) <*> (brackets typeAnnotation)

termApply :: LCParser
termApply = chainl1 (notApply <|> parens termApply) $ TApp <$> getPosition

notApply :: LCParser
notApply = try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)

typeAbstraction :: LCParser
typeAbstraction = parens $ do
    p <- getPosition <* reserved "lambda"
    x <- ucid
    s <- getState
    withState (addName x) $ do
      oty <- optionalType <* dot
      t <- notApply
      return $ TTAbs p x oty t

abstraction :: LCParser
abstraction = do
    pos <- getPosition <* reserved "lambda"
    name <- identifier
    ty <- colon *> typeAnnotation <* dot
    withState (addVar name ty) $ TAbs pos name ty <$> term

variable :: LCParser
variable = do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context " <> " " <> (show pos)

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name *> (t <$> getPosition)

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") $> TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = try topAnnotation <|> try universalType <|> typeVarOrID

universalType :: LCTypeParser
universalType = do
    x <- reserved "All" *> ucid
    oty <- optionalType <* dot
    withState (addName x) $ TyAll x oty <$> typeAnnotation

optionalType :: LCTypeParser
optionalType = try (reservedOp "<:" *> typeAnnotation) <|> return TyTop

topAnnotation :: LCTypeParser
topAnnotation = reserved "Top"  $> TyTop

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    names <- getState
    case findVarName names name of
         Just n -> return $ TyVar n (length names)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context "
