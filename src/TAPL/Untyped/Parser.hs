{-# LANGUAGE FlexibleContexts #-}

module TAPL.Untyped.Parser (parse) where

import TAPL.Untyped.Types
import TAPL.Untyped.Context
import TAPL.Untyped.Lexer

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intercalate, all, nub, (\\))
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

type LCParser = Parsec String (UntypedContext Term) Term

parse :: String -> String -> Either ParseError (UntypedContext AST)
parse = runParser untypedParser pureContext
  where pureContext = UntypedContext withoutNames unit
        withoutNames = []
        unit = TVar Info { row = 0, column = 0 } (-1) (-1)

untypedParser :: Parsec String (UntypedContext Term) (UntypedContext AST)
untypedParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of UntypedContext names _ -> UntypedContext names ast)

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
    dot
    optional spaces
    context <- getState
    modifyState $ \c -> addName c varName
    t <- term
    setState context
    return $ TAbs (infoFrom pos) varName t

variable :: LCParser
variable = do
    name <- identifier
    context <- getState
    let ns = names context
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar (infoFrom pos) n (length $ ns)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)
