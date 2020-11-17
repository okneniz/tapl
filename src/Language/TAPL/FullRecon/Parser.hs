module Language.TAPL.FullRecon.Parser (parse) where

import Language.TAPL.FullRecon.Types
import Language.TAPL.FullRecon.Context
import Language.TAPL.FullRecon.Lexer
import Language.TAPL.Common.Helpers (ucid, padded, withState)

import Text.Parsec (SourcePos)
import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.Functor (($>))
import Data.List (findIndex)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser reconParser []

reconParser :: Parsec String LCNames ([Command], LCNames)
reconParser = (,) <$> (command `sepEndBy` semi <* eof ) <*> getState

command :: Parsec String LCNames Command
command = bindCommand <|> evalCommand

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ VarBind ty

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = apply <|> notApply <|> parens term

apply :: LCParser
apply = chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = value
       <|> (isZero <?> "zero?")
       <|> (condition <?> "condition")
       <|> (letT <?> "let")
       <|> (variable <?> "variable")
       <|> try (parens notApply)

letT :: LCParser
letT = do
    p <- reserved "let" *> getPosition
    v <- identifier
    t1 <- reservedOp "=" *> term <* reserved "in"
    withState (addName v) $ TLet p v t1 <$> term

value :: LCParser
value = (boolean <?> "boolean")
    <|> (nat <?> "nat")
    <|> (abstraction <?> "abstraction")

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    name <- identifier
    ty <- optionMaybe (colon *> typeAnnotation) <* dot
    withState (addName name) $ TAbs pos name ty <$> term

variable :: LCParser
variable = do
    name <- identifier
    ns <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar  pos n (length ns)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context " <> " " <> (show pos)

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

nat :: LCParser
nat = succ <|> pred <|> zero <|> integer
    where succ = fun "succ" TSucc
          pred = fun "pred" TPred
          zero = constant "zero" TZero
          integer = do
            p <- getPosition
            i <- try natural
            toNat p i (TZero p)
          toNat _ i _ | i < 0 = unexpected "unexpected negative number"
          toNat _ 0 t = return t
          toNat p i t = toNat p (i - 1) (TSucc p t)

isZero :: LCParser
isZero = fun "zero?" TIsZero

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> term

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name *> (t <$> getPosition)

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") $> TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation <|> natAnnotation <|> typeVarOrID

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name $> ty

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    ns <- getState
    return $ case findIndex ((== name) . fst) ns of
                  Just varName -> TyVar varName (length ns)
                  Nothing -> TyID name
