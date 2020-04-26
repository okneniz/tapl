module Language.TAPL.Recon.Parser (parse) where

import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Context
import Language.TAPL.Recon.Lexer

import Prelude hiding (abs, succ, pred)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.List (findIndex)

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

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

command :: Parsec String LCNames Command
command = (try evalCommand) <|> bindCommand

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    _ <- spaces
    reserved "="
    modifyState $ \c -> addName c (i:d)
    ty <- typeAnnotation
    return $ Bind (infoFrom pos) (i:d) $ VarBind ty

evalCommand :: LCCommandParser
evalCommand = do
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
            return $ TApp (infoFrom pos)

notApply :: LCParser
notApply = try value
       <|> try (condition <?> "condition")
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)
       <|> try (parens apply)

notTypeBind :: LCParser
notTypeBind = try apply
          <|> try notApply
          <|> try (parens notTypeBind)

value :: LCParser
value = (boolean <?> "boolean")
    <|> (succ <?> "succ")
    <|> (pred <?> "pred")
    <|> (isZero <?> "zero?")
    <|> (zero <?> "zero")

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    name <- identifier
    ty <- termType
    _ <- dot
    optional spaces
    names <- getState
    modifyState $ bind name (VarBind ty)
    t <- notTypeBind
    setState names
    return $ TAbs (infoFrom pos) name ty t

variable :: LCParser
variable = do
    name <- identifier
    ns <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar (infoFrom pos) n (length ns)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (Info -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- notTypeBind
    return $ tm (infoFrom p) t

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

constant :: String -> (Info -> Term) -> LCParser
constant name t = do
    p <- getPosition
    reserved name
    return $ t (infoFrom p)

zero :: LCParser
zero = constant "zero" TZero

condition :: LCParser
condition = do
    reserved "if"
    x <- notTypeBind
    reserved "then"
    y <- notTypeBind
    reserved "else"
    z <- notTypeBind
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

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
notArrowAnnotation = booleanAnnotation
                 <|> try natAnnotation
                 <|> try typeVarOrID

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty

typeVarOrID:: LCTypeParser
typeVarOrID = do
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    ns <- getState
    let name = (i:d)
    return $ case  findIndex ((== name) . fst) ns of
                  Just varName -> TyVar varName (length ns)
                  Nothing -> TyID name
