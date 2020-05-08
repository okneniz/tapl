module Language.TAPL.TypedArith.Types where

data Term = TTrue Info
          | TFalse Info
          | TIf Info Term Term Term
          | TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          | TZero Info
          | TSucc Info Term
          | TPred Info Term
          | TIsZero Info Term
          deriving (Show)

data Type = TyBool
          | TyArrow Type Type
          | TyNat
          deriving (Eq)

type VarName = Int
type Depth = Int
data Info = Info { row :: Int, column :: Int } deriving (Eq)
type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

instance Show Type where
    show (TyArrow t1 t2) = "("++ show t1 ++ " -> " ++ show t2 ++ ")"
    show TyBool = "Bool"
    show TyNat = "Nat"


isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal x | isNumerical x = True
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t1) = TAbs info x ty (walk (c+1) t1)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk _ (TTrue info) = TTrue info
                  walk _ (TFalse info) = TFalse info
                  walk _ (TZero info) = TZero info
                  walk c (TIsZero info t1) = TIsZero info (walk c t1)
                  walk c (TPred info t1) = TPred info (walk c t1)
                  walk c (TSucc info t1) = TSucc info (walk c t1)

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