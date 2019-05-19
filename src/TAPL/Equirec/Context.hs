{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module TAPL.Equirec.Context where

import TAPL.Equirec.Types
import Data.Maybe (isJust)
import Control.Monad (liftM)
import Data.List (intercalate, findIndex)
import Text.Parsec (SourcePos)

data EquirecContext t = EquirecContext LCNames t

class LCContext c where
  bind :: c -> String -> Binding -> c
  addName :: c -> String -> c
  isBound :: c -> String -> Bool
  pickFreshName :: c -> String -> (String, c)
  pickVar :: c -> VarName -> Maybe (String, Binding)
  nameFromContext :: c -> VarName -> Maybe String
  names :: c -> LCNames
  varName :: c -> String -> Maybe VarName

instance LCContext (EquirecContext Term) where
  bind (EquirecContext n t) x b = EquirecContext ((x,b):n) t
  addName c x = bind c x NameBind
  isBound (EquirecContext n _) name = isJust $ Prelude.lookup name n

  pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
  pickFreshName c name = (name, c') where c' = addName c name

  pickVar (EquirecContext [] _) varname = Nothing
  pickVar (EquirecContext names _) varname | length names > varname = Just $ names !! varname
  pickVar _ _ = Nothing

  nameFromContext c varname = liftM fst $ pickVar c varname
  names (EquirecContext n _) = n
  varName (EquirecContext n _) s = findIndex (\(x,_) -> x == s) n

instance Show (EquirecContext Term) where
  show c@(EquirecContext n (TVar _ varname depth)) =
      case nameFromContext c varname of
           Just s -> s
           _ -> "[bad index in " ++ show varname ++ "]"

  show c@(EquirecContext n (TAbs _ name _ t)) =
      let (name', c') = pickFreshName (EquirecContext n t) name
      in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

  show (EquirecContext n (TApp _ t1 t2)) = show (EquirecContext n t1) ++ " " ++ show (EquirecContext n t2)

instance Show (EquirecContext AST) where
  show (EquirecContext n ast) = intercalate ";" $ (\t -> show $ EquirecContext n t) <$> ast


instance LCContext (EquirecContext Type) where
  bind (EquirecContext n t) x b = EquirecContext ((x,b):n) t
  addName c x = bind c x NameBind
  isBound (EquirecContext n _) name = isJust $ Prelude.lookup name n

  pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
  pickFreshName c name = (name, c') where c' = addName c name

  pickVar (EquirecContext [] _) varname = Nothing
  pickVar (EquirecContext names _) varname | length names > varname = Just $ names !! varname
  pickVar _ _ = Nothing

  nameFromContext c varname = liftM fst $ pickVar c varname
  names (EquirecContext n _) = n
  varName (EquirecContext n _) s = findIndex (\(x,_) -> x == s) n

instance Show (EquirecContext Type) where
    show c@(EquirecContext n (TyArrow ty1 ty2)) =
        (show $ EquirecContext n ty1) ++ " -> " ++ (show $ EquirecContext n ty2)

    show c@(EquirecContext n (TyID s)) = s

    show c@(EquirecContext n (TyRec s ty)) = "Rec " ++ s ++ "." ++ (show $ EquirecContext n ty)

    show c@(EquirecContext n (TyVar name ty)) =
         case nameFromContext c name of
              Just s -> s
              _ -> "[bad index in " ++ show name ++ "]"
