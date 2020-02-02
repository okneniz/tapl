module Language.TAPL.Untyped.Parser (parse) where

import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Context
import Language.TAPL.Untyped.Lexer

import Prelude hiding (abs, succ, pred)
import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)
import Data.List (findIndex)

type LCParser = Parsec String LCNames Term

parse :: String -> String -> Either ParseError (AST, LCNames)
parse = runParser untypedParser []

untypedParser :: Parsec String LCNames (AST, LCNames)
untypedParser = do
    ast <- term `sepEndBy` semi
    eof
    names <- getState
    return (ast, names)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp (infoFrom pos)

notApply :: LCParser
notApply = (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> (parens notApply)

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    varName <- identifier
    _ <- dot
    optional spaces
    context <- getState
    modifyState $ \c -> addName c varName
    t <- term
    setState context
    return $ TAbs (infoFrom pos) varName t

variable :: LCParser
variable = do
    name <- identifier
    ns <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar (infoFrom pos) n (length $ ns)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)
