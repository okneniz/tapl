module Language.TAPL.FullSub.TypeChecker (typeOf) where

import Prelude hiding (abs, succ, pred)
import Data.List (tails, intercalate, all, (\\), sort)
import qualified Data.Map.Lazy as Map
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Text.Parsec (SourcePos)

import Control.Monad (liftM, liftM2, when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullSub.Types
import Language.TAPL.FullSub.Pretty
import Language.TAPL.FullSub.Context

data TypeError = TypeMissmatch SourcePos String

typeOf :: Term -> Eval Type
--  | TmVar(fi,i,_) -> getTypeFromContext fi ctx i
typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError p "var type error"

--  | TmAbs(fi,x,tyT1,t2) ->
--      let ctx' = addbinding ctx x (VarBind(tyT1)) in
--      let tyT2 = typeof ctx' t2 in
--      TyArr(tyT1, typeShift (-1) tyT2)

typeOf (TAbs _ x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

--  | TmApp(fi,t1,t2) ->
--      let tyT1 = typeof ctx t1 in
--      let tyT2 = typeof ctx t2 in
--      (match simplifyty ctx tyT1 with
--          TyArr(tyT11,tyT12) ->
--            if subtype ctx tyT2 tyT11 then tyT12
--            else error fi "parameter type mismatch"
--        | _ -> error fi "arrow type expected")

typeOf (TApp p t1 t2) = do
    ty1 <- simplifyType =<< typeOf t1
    ty2 <- simplifyType =<< typeOf t2
    case ty1 of
         (TyArrow ty1' ty2') -> do
            unlessM (ty2 <: ty1') $ do
                ty1p <- prettifyType ty1
                ty2p <- prettifyType ty2
                typeError p $ "incorrect application " ++ show ty2p ++ " to " ++ show ty1p
            return ty2'
         _ -> do
            ty1p <- prettifyType ty1
            typeError p $ "arrow type expected, insted" ++ show ty1p

--  | TmTrue(fi) ->
--      TyBool
--  | TmFalse(fi) ->
--      TyBool

typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

--  | TmIf(fi,t1,t2,t3) ->
--      if subtype ctx (typeof ctx t1) TyBool then
--        join ctx (typeof ctx t2) (typeof ctx t3)
--      else error fi "guard of conditional not a boolean"

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: TyBool)
            (typeError p $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty1 ++ ")")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    joinTypes ty2 ty3

--  | TmRecord(fi, fields) ->
--      let fieldtys =
--        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
--      TyRecord(fieldtys)

typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = (,) <$> return k <*> typeOf v

--  | TmProj(fi, t1, l) ->
--      (match simplifyty ctx (typeof ctx t1) with
--          TyRecord(fieldtys) ->
--            (try List.assoc l fieldtys
--             with Not_found -> error fi ("label "^l^" not found"))
--        | _ -> error fi "Expected record type")

typeOf (TProj _ t (TKeyword p key)) = do
    ty <- simplifyType =<< typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "label " ++ show key ++ " not found"
         _ -> typeError p "expected record type"

typeOf (TProj p _ _) = typeError p "invalid projection"

--  | TmLet(fi,x,t1,t2) ->
--     let tyT1 = typeof ctx t1 in
--     let ctx' = addbinding ctx x (VarBind(tyT1)) in
--     typeShift (-1) (typeof ctx' t2)

typeOf (TLet _ x t1 t2) = do
    ty1 <- typeOf t1
    withTmpStateT (addVar x ty1) $ do
        ty2 <- typeOf t2
        return $ typeShift (-1) ty2

--  | TmFix(fi, t1) ->
--      let tyT1 = typeof ctx t1 in
--      (match simplifyty ctx tyT1 with
--           TyArr(tyT11,tyT12) ->
--             if subtype ctx tyT12 tyT11 then tyT12
--             else error fi "result of body not compatible with domain"
--         | _ -> error fi "arrow type expected")

typeOf (TFix p t1) = do
    tyT1 <- simplifyType =<< typeOf t1
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            unlessM (tyT12 <: tyT11)
                    (typeError p  "result of body not compatible with domain")
            return tyT12
         _ -> typeError p  "arrow type expected"

--  | TmString _ -> TyString

typeOf (TString _ _) = return TyString

--  | TmUnit(fi) -> TyUnit

typeOf (TUnit _) = return TyUnit

--  | TmAscribe(fi,t1,tyT) ->
--     if subtype ctx (typeof ctx t1) tyT then
--       tyT
--     else
--       error fi "body of as-term does not have the expected type"

typeOf (TAscribe p t ty) = do
    ty' <- typeOf t
    unlessM (ty' <: ty) (typeError p "body of as-term does not have the expected type")
    return ty

--  | TmFloat _ -> TyFloat

typeOf (TFloat _ _) = return TyFloat

--  | TmTimesfloat(fi,t1,t2) ->
--      if subtype ctx (typeof ctx t1) TyFloat
--      && subtype ctx (typeof ctx t2) TyFloat then TyFloat
--      else error fi "argument of timesfloat is not a number"

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: TyFloat) (unexpectedType p TyFloat ty1)
    ty2 <- typeOf t2
    unlessM (ty2 <: TyFloat) (unexpectedType p TyFloat ty2)
    return TyFloat

--  | TmZero(fi) ->
--      TyNat

typeOf (TZero _) = return TyNat

--  | TmSucc(fi,t1) ->
--      if subtype ctx (typeof ctx t1) TyNat then TyNat
--      else error fi "argument of succ is not a number"

typeOf (TSucc p t) = do
  ty <- typeOf t
  unlessM (ty <: TyNat) (unexpectedType p TyNat ty)
  return TyNat

--  | TmPred(fi,t1) ->
--      if subtype ctx (typeof ctx t1) TyNat then TyNat
--      else error fi "argument of pred is not a number"

typeOf (TPred p t) = do
  ty <- typeOf t
  unlessM (ty <: TyNat) (unexpectedType p TyNat ty)
  return TyNat

--  | TmIsZero(fi,t1) ->
--      if subtype ctx (typeof ctx t1) TyNat then TyBool
--      else error fi "argument of iszero is not a number"

typeOf (TIsZero p t) = do
  ty <- typeOf t
  unlessM (ty <: TyNat) (unexpectedType p TyNat ty)
  return TyBool

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

unexpectedType :: SourcePos -> Type -> Type -> Eval a
unexpectedType p expected actual = do
    tyE <- prettifyType expected
    tyA <- prettifyType actual
    typeError p $ "expected type " ++ show tyE ++ ", actual " ++ show tyA

instance Show TypeError where
    show (TypeMissmatch p message) = show p ++ ":" ++ message

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    ty1' <- simplifyType ty1
    ty2' <- simplifyType ty2
    n <- get
    case (ty1', ty2') of
      (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
      (TyString, TyString) -> return True
      (TyTop, TyTop) -> return True
      (TyUnit, TyUnit) -> return True
      ((TyID x), (TyID y)) -> return $ x == y
      (TyFloat, TyFloat) -> return True

      (TyVar _ i, _) | isTypeAbb n i -> do
            case getTypeAbb n i of
                Just x -> typeEq x ty2'
                _ -> return False

      (_, TyVar _ i) | isTypeAbb n i -> do
            case getTypeAbb n i of
                Just x -> typeEq x ty1'
                _ -> return False

      (TyVar _ i, TyVar _ j) | i == j -> return True

      (TyBool, TyBool) -> return True
      (TyNat, TyNat) -> return True

      (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
      (TyRecord f1, TyRecord f2) ->
        all (id) <$> sequence (uncurry typeEq <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

      _ -> return False

--let rec subtype ctx tyS tyT =
--   tyeqv ctx tyS tyT ||
--   let tyS = simplifyty ctx tyS in
--   let tyT = simplifyty ctx tyT in
--   match (tyS,tyT) with
--     (_,TyTop) ->
--       true
--   | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
--       (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
--   | (TyRecord(fS), TyRecord(fT)) ->
--       List.for_all
--         (fun (li,tyTi) ->
--            try let tySi = List.assoc li fS in
--                subtype ctx tySi tyTi
--            with Not_found -> false)
--         fT
--   | (_,_) ->
--       false

(<:) :: Type -> Type -> Eval Bool
(<:) tyS tyT = do
    x <- typeEq tyS tyT
    if x
    then return True
    else do
        tyS' <- simplifyType tyS
        tyT' <- simplifyType tyT
        case (tyS', tyT) of
              (_, TyTop) -> return True
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyS1 <: tyT1) <*> (tyS2 <: tyT2)
              (TyRecord f1, TyRecord f2) ->
                all (id) <$> (sequence (uncurry (<:) <$> (Map.elems $ Map.intersectionWith (,) f1 f2)))
              _ -> return False
    where subs f1 f2 = uncurry (<:) <$> fs f1 f2
          fs f1 f2 = Map.elems $ Map.intersectionWith (,) f1 f2

--let rec join ctx tyS tyT =
--  if subtype ctx tyS tyT then tyT else
--  if subtype ctx tyT tyS then tyS else
--  let tyS = simplifyty ctx tyS in
--  let tyT = simplifyty ctx tyT in
--  match (tyS,tyT) with
--    (TyRecord(fS), TyRecord(fT)) ->
--      let labelsS = List.map (fun (li,_) -> li) fS in
--      let labelsT = List.map (fun (li,_) -> li) fT in
--      let commonLabels =
--        List.find_all (fun l -> List.mem l labelsT) labelsS in
--      let commonFields =
--        List.map (fun li ->
--                    let tySi = List.assoc li fS in
--                    let tyTi = List.assoc li fT in
--                    (li, join ctx tySi tyTi))
--                 commonLabels in
--      TyRecord(commonFields)
--  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
--      (try TyArr(meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
--        with Not_found -> TyTop)
--  | _ ->
--      TyTop

joinTypes :: Type -> Type -> Eval Type
joinTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyT
    else do y <- tyT <: tyS
            if y
            then return tyS
            else do z <- (,) <$> simplifyType tyS <*> simplifyType tyT
                    case z of
                         (TyRecord fS, TyRecord fT) -> do
                            fTS <- sequence $ fmap f $ Map.toList $ Map.intersectionWith (,) fS fT
                            return $ TyRecord (Map.fromList fTS)
                            where f (k, (tyS, tyT)) = ((,) k) <$> joinTypes tyS tyT

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            j <- meetTypes tyS1 tyT1
                            case j of
                                 Just ty -> TyArrow ty <$> joinTypes tyS2 tyT2
                                 Nothing -> return TyTop

                         _ -> return TyTop

--meet ctx tyS tyT =
--  if subtype ctx tyS tyT then tyS else
--  if subtype ctx tyT tyS then tyT else
--  let tyS = simplifyty ctx tyS in
--  let tyT = simplifyty ctx tyT in
--  match (tyS,tyT) with
--    (TyRecord(fS), TyRecord(fT)) ->
--      let labelsS = List.map (fun (li,_) -> li) fS in
--      let labelsT = List.map (fun (li,_) -> li) fT in
--      let allLabels =
--        List.append
--          labelsS
--          (List.find_all
--            (fun l -> not (List.mem l labelsS)) labelsT) in
--      let allFields =
--        List.map (fun li ->
--                    if List.mem li allLabels then
--                      let tySi = List.assoc li fS in
--                      let tyTi = List.assoc li fT in
--                      (li, meet ctx tySi tyTi)
--                    else if List.mem li labelsS then
--                      (li, List.assoc li fS)
--                    else
--                      (li, List.assoc li fT))
--                 allLabels in
--      TyRecord(allFields)
--  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
--      TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
--  | _ ->
--      raise Not_found

ok x = (return.return) x
nvm = return Nothing

meetTypes :: Type -> Type -> Eval (Maybe Type)
meetTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then ok tyS
    else do y <- tyT <: tyS
            if y
            then ok tyT
            else do z <- (,) <$> simplifyType tyS <*> simplifyType tyT
                    case z of
                         (TyRecord fS, TyRecord fT) -> do
                            fST <- sequence $ fmap (uncurry f)
                                            $ Map.toList
                                            $ merge (mapMaybeMissing $ \k ty -> return (Just ty, Nothing))
                                                    (mapMaybeMissing $ \k ty -> return (Nothing, Just ty))
                                                    (zipWithMaybeMatched $ \k tySi tyTi -> return (Just tySi, Just tyTi))
                                                    fS fT
                             -- TODO : WTF ? https://github.com/enaudon/TAPL/blob/master/source/fullsub/core.ml#L240
                             -- what is Nothing (not found in original implementation) in this case?
                             -- how to handle it?
                            ok $ TyRecord $ Map.fromList $ catMaybes fST
                            where f k (Just tySi, Just tyTi) = liftM ((,) k) <$> meetTypes tySi tyTi
                                  f k (Just tySi, Nothing) = ok (k, tySi)
                                  f k (Nothing, Just tyTi) = ok (k, tyTi)

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            j <- (,) <$> joinTypes tyS1 tyT1 <*> meetTypes tyS2 tyT2
                            case j of
                                 (ty1, Just ty2) -> ok $ TyArrow ty1 ty2
                                 (ty1, Nothing) -> nvm

                         _ -> nvm
