module Language.TAPL.Untyped.Types where

data Term = TVar Info VarName Depth
          | TAbs Info String Term
          | TApp Info Term Term
          deriving (Show)

type VarName = Int
type Depth = Int
data Info = Info { row :: Int, column :: Int } deriving (Eq)
type AST = [Term]

data Binding = NameBind deriving (Show)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

isVal :: Term -> Bool
isVal (TAbs _ _ _) = True
isVal _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x t1) = TAbs info x (walk (c+1) t1)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = tmmap onvar s t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar _ info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c _ name _ | name == j + c = shift c s
                     onvar _ info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
