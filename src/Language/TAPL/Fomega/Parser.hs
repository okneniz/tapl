module Language.TAPL.Fomega.Parser (parse) where

import Language.TAPL.Fomega.Types
import Language.TAPL.Fomega.Context
import Language.TAPL.Fomega.Lexer
import Language.TAPL.Common.Helpers (ucid, padded, withState)
import Language.TAPL.Common.Context (findVarName)

import Data.Functor (($>))

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type
type LCKindParser = Parsec String LCNames Kind

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser fOmegaParser []

fOmegaParser :: Parsec String LCNames ([Command], LCNames)
fOmegaParser = (,) <$> (evalCommand `sepEndBy` semi <* eof) <*> getState

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = termApply <|> typeApply <|> notApply <|> (parens term)

termApply :: LCParser
termApply = try $ chainl1 p $ TApp <$> getPosition
    where p = try typeApply <|> try notApply <|> try (parens typeApply) <|> parens termApply

typeApply :: LCParser
typeApply = try $ do
    p <- getPosition
    t <- try typeAbstraction <|> variable
    ty <- z
    try (f $ TTApp p t ty) <|> return (TTApp p t ty)
    where z = bracketType <|> typeAbstractionAnnotation <|> typeVar
          f t1 = do t2 <- TTApp <$> getPosition <*> (return t1) <*> z
                    try (f t2) <|> (return t2)

notApply :: LCParser
notApply = value <|> (variable <?> "variable") <|> (parens notApply)

value :: LCParser
value = (typeAbstraction <?> "type abstraction") <|> (abstraction <?> "abstraction")

typeAbstraction :: LCParser
typeAbstraction = try $ optionalParens $ do
    p <- getPosition
    x <- reserved "lambda" *> ucid
    k <- optionalKind <* dot
    withState (addName x) $ TTAbs p x k <$> term

abstraction :: LCParser
abstraction = try $ optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- colon *> typeAnnotation <* dot
    withState (addVar name ty) $ TAbs pos name ty <$> term

variable :: LCParser
variable = try $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> unexpected $ "variable " <> show name <> " hasn't been bound in context " <> (show names)

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = try $ chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") $> TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = universalType
                 <|> typeAbstractionAnnotation
                 <|> typeApplyAnnotation
                 <|> typeVar

bracketType :: LCTypeParser
bracketType = brackets $ arrowAnnotation <|> typeVar

universalType :: LCTypeParser
universalType = try $ do
    x <- reserved "All" *> ucid
    k <- optionalKind <* dot
    withState (addName x) $ TyAll x k <$> typeAnnotation

typeAbstractionAnnotation :: LCTypeParser
typeAbstractionAnnotation = try $ optionalParens $ do
    x <- reserved "lambda" *> ucid
    k <- optionalKind <* dot
    withState (addName x) $ TyAbs x k <$> typeAnnotation

typeApplyAnnotation :: LCTypeParser
typeApplyAnnotation =
    try $ chainl1 (typeVar <|> bracketType <|> (parens typeApplyAnnotation)) $ optional spaces $> TyApp

typeVar:: LCTypeParser
typeVar = try $ do
    name <- ucid
    names <- getState
    case findVarName names name of
         Just varName -> return $ TyVar varName (length names)
         Nothing -> unexpected $ "type variable " <> show name <> " hasn't been bound in context " <> (show names)

optionalKind :: LCKindParser
optionalKind = (reservedOp "::" *> kindAnnotation) <|> return Star

kindAnnotation :: LCKindParser
kindAnnotation = arrowKind <|> startKind

startKind :: LCKindParser
startKind = reservedOp "*" $> Star

arrowKind :: LCKindParser
arrowKind = chainr1 (startKind <|> parens kindAnnotation) $ padded (reservedOp "->") $> Arrow

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> try f
