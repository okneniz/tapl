module Language.TAPL.Fomsub.Parser (parse) where

import Language.TAPL.Fomsub.Types
import Language.TAPL.Fomsub.Context
import Language.TAPL.Fomsub.Lexer
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
parse = runParser fomSubParser []

fomSubParser :: Parsec String LCNames ([Command], LCNames)
fomSubParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = padded evalCommand

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = termApply
   <|> typeApply
   <|> notApply
   <|> parens term

termApply :: LCParser
termApply = try $ chainl1 (padded p) $ TApp <$> getPosition
    where p = try typeApply <|> try notApply <|> try (parens typeApply) <|> parens termApply

typeApply :: LCParser
typeApply = try $ do
    p <- getPosition
    t <- try typeAbstraction <|> variable
    ty <- z
    try (f $ TTApp p t ty) <|> return (TTApp p t ty)
    where z = bracketType <|> typeAbstractionAnnotation <|> typeVar
          f t1 = do
            t2 <- TTApp <$> getPosition <*> (return t1) <*> z
            try (f t2) <|> (return t2)

notApply :: LCParser
notApply = value <|> variable <|> parens notApply

notTypeBind :: LCParser
notTypeBind = termApply <|> notApply

value :: LCParser
value = (typeAbstraction <?> "type abstraction") <|> (abstraction <?> "abstraction")

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

abstraction :: LCParser
abstraction = try $ optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- colon *> typeAnnotation
    names <- getState
    modifyState $ addVar name ty
    t <- dot *> term
    setState names
    return $ TAbs pos name ty t

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
                 <|> universalType
                 <|> typeAbstractionAnnotation
                 <|> typeApplyAnnotation
                 <|> typeVar

bracketType :: LCTypeParser
bracketType = brackets $ arrowAnnotation
                     <|> universalType
                     <|> typeVar

universalType :: LCTypeParser
universalType = try $ do
    x <- reserved "All" *> ucid
    ty1 <- optionalType
    names <- getState
    modifyState $ addName x
    ty2 <- dot *> typeAnnotation
    setState names
    return $ TyAll x ty1 ty2

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
    try $ chainl1 (typeVar <|> bracketType <|> (parens typeApplyAnnotation)) $ do
        optional spaces
        return TyApp

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

typeVar:: LCTypeParser
typeVar = try $ do
    name <- ucid
    names <- getState
    case findVarName names name of
         Just varName -> return $ TyVar varName (length names)
         Nothing -> unexpected $ "type variable " <> show name <> " hasn't been bound in context " <> (show names)

kindAnnotation :: LCKindParser
kindAnnotation = arrowKind <|> startKind

optionalKind :: LCKindParser
optionalKind = try (reservedOp "::" *> kindAnnotation) <|> return Star

startKind :: LCKindParser
startKind = reservedOp "*" >> return Star

arrowKind :: LCKindParser
arrowKind = chainr1 (startKind <|> parens kindAnnotation) $ padded (reservedOp "->") $> Arrow

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> try f
