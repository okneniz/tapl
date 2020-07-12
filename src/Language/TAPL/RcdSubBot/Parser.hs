module Language.TAPL.RcdSubBot.Parser (parse) where

import Language.TAPL.RcdSubBot.Types
import Language.TAPL.RcdSubBot.Context
import Language.TAPL.RcdSubBot.Lexer

import Prelude hiding (abs, succ, pred)
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Text.Parsec (SourcePos)

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
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    _ <- spaces
    reservedOp "="
    modifyState $ addName (i:d)
    ty <- typeAnnotation
    return $ Bind pos (i:d) $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = try $ do
    ast <- term `sepEndBy` semi
    return $ Eval ast

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
notApply = try value
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)
       <|> try (parens apply)

notTypeBind :: LCParser
notTypeBind = try apply
          <|> try notApply
          <|> try (parens notTypeBind)

value :: LCParser
value = record <?> "record"

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    name <- identifier
    ty <- termType
    _ <- dot
    optional spaces
    names <- getState
    modifyState $ addVar name ty
    t <- notTypeBind
    setState names
    return $ TAbs pos name ty t

variable :: LCParser
variable = projection keyword $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

projection :: LCParser -> LCParser -> LCParser
projection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
  where dotRef k t = do
          _ <- dot
          pos <- getPosition
          i <- k
          t' <- (try $ dotRef key (TProj pos t i)) <|> (return $ TProj pos t i)
          return t'

record :: LCParser
record = projection keyword $ braces $ do
    ts <- (keyValue (reservedOp "=") term) `sepBy` comma
    pos <- getPosition
    return $ TRecord pos $ Map.fromList ts

keyword :: LCParser
keyword = do
  word <- identifier
  p <- getPosition
  return $ TKeyword p word

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = do
  k <- identifier
  _ <- devider
  v <- val
  return (k,v)

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    optional spaces
    reservedOp "->"
    optional spaces
    return $ TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = recordAnnotation
                 <|> try topAnnotation
                 <|> try botAnnotation

recordAnnotation :: LCTypeParser
recordAnnotation = braces $ do
    tys <- (keyValue colon typeAnnotation) `sepBy` comma
    return $ TyRecord $ Map.fromList tys

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty
