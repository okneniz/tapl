module Language.TAPL.FullFomSub.TypeChecker (typeOf, kindOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate, sort)
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Control.Monad (when, unless, liftM, liftM2, when, unless, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.FullFomSub.Pretty (render, renderType)

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Context
import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullFomSub.Types
import Language.TAPL.FullFomSub.Context

typeOf :: Term -> Eval Type
--TmVar(fi,i,_) -> getTypeFromContext fi ctx i
typeOf (TVar p v _) = do
    n <- get
    b <- getBinding p n v
    case b of
         (Just (VarBind ty))-> return ty
         (Just x)-> typeError p $ "wrong kind of binding for variable " ++ show x
         Nothing -> typeError p "var type error"

--  | TmAbs(fi,x,tyT1,t2) ->
--      checkkindstar fi ctx tyT1;
--      let ctx' = addbinding ctx x (VarBind(tyT1)) in
--      let tyT2 = typeof ctx' t2 in
--      TyArr(tyT1, typeShift (-1) tyT2)

typeOf (TAbs p x tyT1 t2) = do
    unlessM (isStar p tyT1) (typeError p "Kind * expected")
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        TyArrow tyT1 <$> typeShift p (-1) tyT2

--  | TmApp(fi,t1,t2) ->
--      let tyT1 = typeof ctx t1 in
--      let tyT2 = typeof ctx t2 in
--      (match lcst ctx tyT1 with
--          TyArr(tyT11,tyT12) ->
--            if subtype ctx tyT2 tyT11 then tyT12
--            else error fi "parameter type mismatch"
--        | _ -> error fi "arrow type expected")

typeOf r@(TApp p t1 t2) = do
    tyT1 <- lcst p =<< typeOf t1
    tyT2 <- typeOf t2
    n <- get
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            tyT22 <- simplifyType p tyT2
            unlessM (isSubtype p tyT2 tyT11) (typeError p $ "parameter type missmatch " ++ show tyT2 ++ "  " ++ show tyT11)
            return tyT12
         x -> typeError p $ "arrow type expected " ++ show x ++ " : " ++ show n

--  | TmTrue(fi) ->
--      TyBool
typeOf (TTrue _) = return TyBool

--  | TmFalse(fi) ->
--      TyBool
typeOf (TFalse _) = return TyBool

--  | TmIf(fi,t1,t2,t3) ->
--      if subtype ctx (typeof ctx t1) TyBool then
--        join ctx (typeof ctx t2) (typeof ctx t3)
--      else error fi "guard of conditional not a boolean"
typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (isSubtype p ty1 TyBool) (typeError p $ "guard of conditional not a boolean")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    joinTypes p ty2 ty3

--  | TmRecord(fi, fields) ->
--      let fieldtys =
--        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
--      TyRecord(fieldtys)
typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = (,) k <$> typeOf v

--  | TmProj(fi, t1, l) ->
--      (match lcst ctx (typeof ctx t1) with
--          TyRecord(fieldtys) ->
--            (try List.assoc l fieldtys
--             with Not_found -> error fi ("label "^l^" not found"))
--        | _ -> error fi "Expected record type")

typeOf s@(TProj _ t (TKeyword p key)) = do
    ty <- lcst p =<< typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         x -> typeError p $ "Expected record type: " ++ show s

typeOf (TProj p _ _) = typeError p "invalid lookup operation"

--  | TmLet(fi,x,t1,t2) ->
--     let tyT1 = typeof ctx t1 in
--     let ctx' = addbinding ctx x (VarBind(tyT1)) in
--     typeShift (-1) (typeof ctx' t2)

typeOf (TLet p x t1 t2) = do
    n <- get
    ty1 <- typeOf t1
    withTmpStateT (addVar x ty1) $ typeShift p (-1) =<< typeOf t2

--  | TmFix(fi, t1) ->
--      let tyT1 = typeof ctx t1 in
--      (match lcst ctx tyT1 with
--           TyArr(tyT11,tyT12) ->
--             if subtype ctx tyT12 tyT11 then tyT12
--             else error fi "result of body not compatible with domain"
--         | _ -> error fi "arrow type expected")

typeOf (TFix p t1) = do
    tyT1 <- lcst p =<< typeOf t1
    case tyT1 of
        (TyArrow tyT11 tyT12) -> do
            unlessM (isSubtype p tyT12 tyT11) (typeError p "result of body not compatible with domain")
            return tyT12
        _ -> typeError p  "arrow type expected"

--  | TmString _ -> TyString
typeOf (TString _ _) = return TyString

--  | TmUnit(fi) -> TyUnit
typeOf (TUnit _) = return TyUnit

--  | TmAscribe(fi,t1,tyT) ->
--     checkkindstar fi ctx tyT;
--     if subtype ctx (typeof ctx t1) tyT then
--       tyT
--     else
--       error fi "body of as-term does not have the expected type"

typeOf (TAscribe p t1 tyT) = do
    unlessM (isStar p tyT) (typeError p "Kind * expected")
    ty1 <- typeOf t1
    unlessM (isSubtype p ty1 tyT) (typeError p "body of as-term does not have the expected type")
    return tyT

--  | TmFloat _ -> TyFloat
typeOf (TFloat _ _) = return TyFloat

--  | TmTimesfloat(fi,t1,t2) ->
--      if subtype ctx (typeof ctx t1) TyFloat
--      && subtype ctx (typeof ctx t2) TyFloat then TyFloat
--      else error fi "argument of timesfloat is not a number"
typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    unlessM (isSubtype p ty1 TyFloat) (argumentError p TyFloat ty1)
    ty2 <- typeOf t2
    unlessM (isSubtype p ty2 TyFloat) (argumentError p TyFloat ty2)
    return TyFloat

--  | TmTAbs(fi,tyX,tyT1,t2) ->
--      let ctx = addbinding ctx tyX (TyVarBind(tyT1)) in
--      let tyT2 = typeof ctx t2 in
--      TyAll(tyX,tyT1,tyT2)
typeOf (TTAbs p x ty t) = withTmpStateT (addTypeVar x ty) $ TyAll x ty <$> typeOf t

--  | TmTApp(fi,t1,tyT2) ->
--      let tyT1 = typeof ctx t1 in
--      (match lcst ctx tyT1 with
--           TyAll(_,tyT11,tyT12) ->
--             if not(subtype ctx tyT2 tyT11) then
--                  error fi "type parameter type mismatch";
--             typeSubstTop tyT2 tyT12
--         | _ -> error fi "universal type expected")

typeOf (TTApp p t1 tyT2) = do
    tyT1 <- lcst p =<< typeOf t1
    case tyT1 of
        TyAll _ tyT11 tyT12 -> do
            unlessM (isSubtype p tyT2 tyT11) (typeError p "type parameter type mismatch")
            typeSubstitutionTop p tyT2 tyT12
        _ -> typeError p "universal type expected"

--  | TmZero(fi) ->
--      TyNat
typeOf (TZero _) = return TyNat

--  | TmSucc(fi,t1) ->
--      if subtype ctx (typeof ctx t1) TyNat then TyNat
--      else error fi "argument of succ is not a number"
typeOf (TSucc p t) = do
    ty <- typeOf t
    unlessM (isSubtype p ty TyNat) (argumentError p TyNat ty)
    return TyNat

--  | TmPred(fi,t1) ->
--      if subtype ctx (typeof ctx t1) TyNat then TyNat
--      else error fi "argument of pred is not a number"
typeOf (TPred p t) = do
    ty <- typeOf t
    unlessM (isSubtype p ty TyNat) (argumentError p TyNat ty)
    return TyNat

--  | TmIsZero(fi,t1) ->
--      if subtype ctx (typeof ctx t1) TyNat then TyBool
--      else error fi "argument of iszero is not a number"
typeOf (TIsZero p t) = do
    ty <- typeOf t
    unlessM (isSubtype p ty TyNat) (argumentError p TyNat ty)
    return TyBool

--  | TmPack(fi,tyT1,t2,tyT) ->
--      checkkindstar fi ctx tyT;
--      (match simplifyty ctx tyT with
--          TySome(tyY,tyBound,tyT2) ->
--            if not (subtype ctx tyT1 tyBound) then
--              error fi "hidden type not a subtype of bound";
--            let tyU = typeof ctx t2 in
--            let tyU' = typeSubstTop tyT1 tyT2 in
--            if subtype ctx tyU tyU' then tyT
--            else error fi "doesn't match declared type"
--        | _ -> error fi "existential type expected")
typeOf (TPack p tyT1 t2 tyT) = do
    unlessM (isStar p tyT) (typeError p "Kind * expected")
    tyTT <- simplifyType p tyT
    case tyTT of
         (TySome tyY tyBound tyT2) -> do
            unlessM (isSubtype p tyT1 tyBound) (typeError p "hidden type not a subtype of bound")
            tyU <- typeOf t2
            tyU' <- typeSubstitutionTop p tyT1 tyT2
            unlessM (isSubtype p tyU tyU') (typeError p $ "doesn\'t match declared type : " ++ show tyU ++ " and " ++ show tyU')
            return tyT

--  | TmUnpack(fi,tyX,x,t1,t2) ->
--      let tyT1 = typeof ctx t1 in
--      (match lcst ctx tyT1 with
--          TySome(tyY,tyBound,tyT11) ->
--            let ctx' = addbinding ctx tyX (TyVarBind tyBound) in
--            let ctx'' = addbinding ctx' x (VarBind tyT11) in
--            let tyT2 = typeof ctx'' t2 in
--            typeShift (-2) tyT2
typeOf (TUnpack p tyX x t1 t2) = do
    tyT1 <- lcst p =<< typeOf t1
    case tyT1 of
         (TySome tyY tyBound tyT11) -> do
            withTmpStateT (addTypeVar tyX tyBound) $ do
                withTmpStateT (addVar x tyT11) $ do
                    typeShift p (-2) =<< typeOf t2
         _ -> typeError p "existential type expected"

isSubtype :: SourcePos -> Type -> Type -> Eval Bool
isSubtype p tyS tyT = do
    x <- typeEq p tyS tyT
    if x
    then return True
    else do
        tyS' <- simplifyType p tyS
        tyT' <- simplifyType p tyT
        case (tyS', tyT') of
              --     (_,TyTop) -> true
              (_, TyTop) -> return True

              --   | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) -> (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (isSubtype p tyT1 tyS1) <*> (isSubtype p tyS2 tyT2)

              --   | (TyRecord(fS), TyRecord(fT)) ->
              --       List.for_all
              --         (fun (li,tyTi) ->
              --            try let tySi = List.assoc li fS in
              --                subtype ctx tySi tyTi
              --            with Not_found -> false)
              --         fT
              (TyRecord f1, TyRecord f2) ->
                all (id) <$> (sequence (uncurry (isSubtype p) <$> (Map.elems $ Map.intersectionWith (,) f1 f2)))

              --   | (TyVar(_,_),_) -> subtype ctx (promote ctx tyS) tyT
              (TyVar _ _, _) -> do
                 x <- promote p tyS'
                 case x of
                      Just ty -> isSubtype p ty tyT
                      Nothing -> return False

              --   | (TyAll(tyX1,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
              --        (subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) &&
              --        let ctx1 = addbinding ctx tyX1 (TyVarBind(tyT1)) in
              --        subtype ctx1 tyS2 tyT2

              (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                    x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                    if x
                    then withTmpStateT (addTypeVar tyX1 tyT1) $ isSubtype p tyS2 tyT2
                    else return False

              --   | (TyAbs(tyX,knKS1,tyS2),TyAbs(_,knKT1,tyT2)) ->
              --        (=) knKS1 knKT1 &&
              --        let ctx = addbinding ctx tyX (TyVarBind(maketop knKS1)) in
              --        subtype ctx tyS2 tyT2
              (TyAbs tyX1 k1 tyS2, TyAbs _ k2 tyT2) | k1 == k2 -> do
                    withTmpStateT (addTypeVar tyX1 (makeTop k1)) $ isSubtype p tyS2 tyT2

              --   | (TyApp(_,_),_) -> subtype ctx (promote ctx tyS) tyT
              (TyApp _ _, _) -> do
                    x <- promote p tyS'
                    case x of
                        Just ty -> isSubtype p ty tyT
                        Nothing -> return False

              --   | (TySome(tyX1,tyS1,tyS2),TySome(_,tyT1,tyT2)) ->
              --        (subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) &&
              --        let ctx1 = addbinding ctx tyX1 (TyVarBind(tyT1)) in
              --        subtype ctx1 tyS2 tyT2
              (TySome tyX1 tyS1 tyS2, TySome _ tyT1 tyT2) -> do
                    x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                    if x
                    then withTmpStateT (addTypeVar tyX1 tyT1) $ isSubtype p tyS2 tyT2
                    else return False

              _ -> return False

--let rec join ctx tyS tyT =
--  if subtype ctx tyS tyT then tyT else
--  if subtype ctx tyT tyS then tyS else
--  let tyS = simplifyty ctx tyS in
--  let tyT = simplifyty ctx tyT in
--  match (tyS,tyT) with
--  | _ -> TyTop

ok x = (return.return) x
notFound = return Nothing

joinTypes :: SourcePos ->  Type -> Type -> Eval Type
joinTypes p tyS tyT = do
    x <- isSubtype p tyS tyT
    if x
    then return tyT
    else do y <- isSubtype p tyT tyS
            if y
            then return tyS
            else do z <- (,) <$> simplifyType p tyS <*> simplifyType p tyT
                    case z of
                         --    (TyRecord(fS), TyRecord(fT)) ->
                         --      let labelsS = List.map (fun (li,_) -> li) fS in
                         --      let labelsT = List.map (fun (li,_) -> li) fT in
                         --      let commonLabels = List.find_all (fun l -> List.mem l labelsT) labelsS in
                         --      let commonFields =
                         --        List.map (fun li ->
                         --                    let tySi = List.assoc li fS in
                         --                    let tyTi = List.assoc li fT in
                         --                    (li, join ctx tySi tyTi))
                         --                 commonLabels in
                         --      TyRecord(commonFields)
                         (TyRecord fS, TyRecord fT) -> do
                            fTS <- sequence $ fmap f $ Map.toList $ Map.intersectionWith (,) fS fT
                            return $ TyRecord (Map.fromList fTS)
                            where f (k, (tyS, tyT)) = ((,) k) <$> joinTypes p tyS tyT

                         --  | (TyAll(tyX,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
                         --      if not(subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) then TyTop
                         --      else
                         --        let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
                         --        TyAll(tyX,tyS1,join ctx' tyT1 tyT2)
                         (TyAll tyX tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                            x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                            if x
                            then withTmpStateT (addTypeVar tyX tyT1) $ TyAll tyX tyS1 <$> joinTypes p tyT1 tyT2
                            else return TyTop

                         --  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
                         --      (try TyArr(meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
                         --        with Not_found -> TyTop)
                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            ty1 <- meetTypes p tyS1 tyT1
                            case ty1 of
                                 Just x -> TyArrow x <$> joinTypes p tyS2 tyT2
                                 Nothing -> return TyTop

                         _ -> return TyTop

meetTypes :: SourcePos -> Type -> Type -> Eval (Maybe Type)
meetTypes p tyS tyT = do
    x <- isSubtype p tyS tyT
    if x
    then ok tyS
    else do y <- isSubtype p tyT tyS
            if y
            then ok tyT
            else do z <- (,) <$> simplifyType p tyS <*> simplifyType p tyT
                    case z of
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
                         (TyRecord fS, TyRecord fT) -> do
                            fST <- sequence $ fmap (uncurry f)
                                            $ Map.toList
                                            $ merge (mapMaybeMissing $ \k ty -> return (Just ty, Nothing))
                                                    (mapMaybeMissing $ \k ty -> return (Nothing, Just ty))
                                                    (zipWithMaybeMatched $ \k tySi tyTi -> return (Just tySi, Just tyTi))
                                                    fS fT
                             -- TODO : WTF ? https://github.com/enaudon/TAPL/blob/master/source/fullsub/core.ml#L240
                             -- what is Nothing (not found in original implementation) in this context?
                             -- how to handle it?
                            ok $ TyRecord $ Map.fromList $ catMaybes fST
                            where f k (Just tySi, Just tyTi) = liftM ((,) k) <$> meetTypes p tySi tyTi
                                  f k (Just tySi, Nothing) = ok (k, tySi)
                                  f k (Nothing, Just tyTi) = ok (k, tyTi)

                         --  | (TyAll(tyX,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
                         --      if not(subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) then
                         --        raise Not_found
                         --      else
                         --        let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
                         --        TyAll(tyX,tyS1,meet ctx' tyT1 tyT2)
                         (TyAll tyX tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                            x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                            if x
                            then notFound
                            else withTmpStateT (addTypeVar tyX tyT1) $ do
                                    y <- meetTypes p tyT1 tyT2
                                    case y of
                                         Just ty -> ok $ TyAll tyX tyS1 ty
                                         Nothing -> notFound

                         --  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) -> TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            j <- (,) <$> joinTypes p tyS1 tyT1 <*> meetTypes p tyS2 tyT2
                            case j of
                                 (ty1, Just ty2) -> ok $ TyArrow ty1 ty2
                                 (ty1, Nothing) -> notFound

                         _ -> notFound

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
    where message = "Argument error, expected " ++ show expected  ++ ". Got " ++ show actual ++ "."

--let checkkindstar fi ctx tyT =
--  let k = kindof ctx tyT in
--  if k = KnStar then ()
--  else error fi "Kind * expected"

isStar :: SourcePos -> Type -> Eval Bool
isStar p ty = (==) Star <$> kindOf p ty

--let rec getkind fi ctx i =
--  match getbinding fi ctx i with
--      TyVarBind(tyT) -> kindof ctx tyT
--    | TyAbbBind(_,Some(knK)) -> knK
--    | TyAbbBind(_,None) -> error fi ("No kind recorded for variable "
--                                      ^ (index2name fi ctx i))
--    | _ -> error fi ("getkind: Wrong kind of binding for variable "
--                      ^ (index2name fi ctx i))

getKind :: SourcePos -> VarName -> Eval Kind
getKind p v = do
    x <- flip(getBinding p) v =<< get
    case x of
        (Just (TypeVarBind ty)) -> kindOf p ty
        (Just (TypeAddBind _ (Just k))) -> return k
        (Just (TypeAddBind _ Nothing)) -> lift $ throwE $ "No kind recorded for variable " ++ show v ++ " : " ++ show x
        _ -> lift $ throwE $ "Wrong kind of binding for variable  " ++ show v ++ " : " ++ show x

kindOf :: SourcePos -> Type -> Eval Kind
--    TyRecord(fldtys) ->
--      List.iter (fun (l,tyS) ->
--             if kindof ctx tyS<>KnStar then error dummyinfo "Kind * expected")
--        fldtys;
--      KnStar

kindOf p (TyRecord fields) = do
    unlessM(all (id) <$> sequence (isStar p <$> (Map.elems fields))) (typeError p "Kind * expected")
    return Star

--  | TyVar(i,_) ->
--      let knK = getkind dummyinfo ctx i
--      in knK

kindOf p (TyVar i _) = getKind p i

--  | TyAll(tyX,tyT1,tyT2) ->
--      let ctx' = addbinding ctx tyX (TyVarBind tyT1) in
--      if kindof ctx' tyT2 <> KnStar then error dummyinfo "Kind * expected";
--      KnStar

kindOf p (TyAll tyX tyT1 tyT2) = do
    withTmpStateT (addTypeVar tyX tyT1) $ do
        unlessM (isStar p tyT2) (typeError p "star kind expected")
        return Star

--  | TyAbs(tyX,knK1,tyT2) ->
--      let ctx' = addbinding ctx tyX (TyVarBind(maketop knK1)) in
--      let knK2 = kindof ctx' tyT2 in
--      KnArr(knK1,knK2)

kindOf p (TyAbs tyX knK1 tyT2) = do
    withTmpStateT (addTypeVar tyX (makeTop knK1)) $ Arrow knK1 <$> kindOf p tyT2

--  | TyApp(tyT1,tyT2) ->
--      let knK1 = kindof ctx tyT1 in
--      let knK2 = kindof ctx tyT2 in
--      (match knK1 with
--          KnArr(knK11,knK12) ->
--            if (=) knK2 knK11 then knK12
--            else error dummyinfo "parameter kind mismatch"
--        | _ -> error dummyinfo "arrow kind expected")

kindOf p x@(TyApp tyT1 tyT2) = do
    knK1 <- kindOf p tyT1
    knK2 <- kindOf p tyT2
    names <- get
    case knK1 of
         (Arrow knK11 knK12) | knK2 == knK11 -> return knK12
         (Arrow knK11 knK12) -> typeError p "parameter kind mismatch"
         _ -> typeError p $ "arrow kind expected " ++ show x ++ " - " ++ show knK1 ++ show knK2 ++ " : " ++ show names

--  | TySome(tyX,tyT1,tyT2) ->
--      let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
--      if kindof ctx' tyT2 <> KnStar then error dummyinfo "Kind * expected";
--      KnStar

kindOf p (TySome tyX tyT1 tyT2) = do
    withTmpStateT (addTypeVar tyX tyT1) $ do
        unlessM (isStar p tyT2) (typeError p "star kind expected")
        return Star

--  | TyArr(tyT1,tyT2) ->
--      if kindof ctx tyT1 <> KnStar then error dummyinfo "star kind expected";
--      if kindof ctx tyT2 <> KnStar then error dummyinfo "star kind expected";
--      KnStar

kindOf p (TyArrow tyT1 tyT2) = do
    unlessM (isStar p tyT1) (typeError p "star kind expected")
    unlessM (isStar p tyT2) (typeError p "star kind expected")
    return Star

kindOf _ _ = return Star

typeEq :: SourcePos -> Type -> Type -> Eval Bool
typeEq p t1 t2 = do
    tyS <- simplifyType p t1
    tyT <- simplifyType p t2
    n <- get
    case (tyS, tyT) of
         --    (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) -> (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2

         --  | (TyString,TyString) -> true
         (TyString, TyString) -> return True

         --  | (TyTop,TyTop) -> true
         (TyTop, TyTop) -> return True

         --  | (TyUnit,TyUnit) -> true
         (TyUnit, TyUnit) -> return True

         --  | (TyId(b1),TyId(b2)) -> b1=b2
         (TyID x, TyID y) -> return $ x == y

         --  | (TyFloat,TyFloat) -> true
         (TyFloat, TyFloat) -> return True

         --  | (TyVar(i,_), _) when istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
         --  | (_, TyVar(i,_)) when istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
         --  | (TyVar(i,_),TyVar(j,_)) -> i=j

         (TyVar i _, TyVar j _) -> do
             bi <- isTypeAdd p n i
             bj <- isTypeAdd p n j
             case (tyS, tyT) of
                  (_, _) | bi -> do
                     m <- getTypeAbb p n i
                     case m of
                          Just x -> typeEq p x tyT
                          _ -> return False

                  (_, _) | bj -> do
                     m <- getTypeAbb p n j
                     case m of
                          Just x -> typeEq p x tyS
                          _ -> return False

                  (TyVar i _, TyVar j _) -> return $ i == j

         --  | (TyBool,TyBool) -> true
         (TyBool, TyBool) -> return True

         --  | (TyNat,TyNat) -> true
         (TyNat, TyNat) -> return True

         --  | (TyRecord(fields1),TyRecord(fields2)) ->
         --       List.length fields1 = List.length fields2
         --       &&
         --       List.for_all
         --         (fun (li2,tyTi2) ->
         --            try let (tyTi1) = List.assoc li2 fields1 in
         --                tyeqv ctx tyTi1 tyTi2
         --            with Not_found -> false)
         --         fields2
         (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
         (TyRecord f1, TyRecord f2) -> all (id) <$> sequence (uncurry (typeEq p) <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

         --  | (TyAll(tyX1,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
         --       let ctx1 = addname ctx tyX1 in
         --       tyeqv ctx tyS1 tyT1 && tyeqv ctx1 tyS2 tyT2

         (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> withTmpStateT (addName tyX1) $ do
             (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2

         --  | (TySome(tyX1,tyS1,tyS2),TySome(_,tyT1,tyT2)) ->
         --       let ctx1 = addname ctx tyX1 in
         --       tyeqv ctx tyS1 tyT1 && tyeqv ctx1 tyS2 tyT2

         (TySome tyX1 tyS1 tyS2, TySome _ tyT1 tyT2) -> withTmpStateT (addName tyX1) $ do
             (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2

         --  | (TyAbs(tyX1,knKS1,tyS2),TyAbs(_,knKT1,tyT2)) ->
         --       ((=) knKS1 knKT1)
         --      &&
         --       (let ctx = addname ctx tyX1 in
         --        tyeqv ctx tyS2 tyT2)
         (TyAbs x k1 ty1, TyAbs _ k2 ty2) | k1 == k2 -> withTmpStateT (addName x) $ typeEq p ty1 ty2

         --  | (TyApp(tyS1,tyS2),TyApp(tyT1,tyT2)) ->
         --       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
         (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2

         (TyKeyword, TyKeyword) -> return True
         _ -> return False

--let rec computety ctx tyT = match tyT with
--  |  TyApp(TyAbs(_,_,tyT12),tyT2) -> typeSubstTop tyT2 tyT12
--  |  TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
--  | _ -> raise NoRuleApplies

computeType :: SourcePos -> Type -> Eval (Maybe Type)
computeType p (TyApp (TyAbs _ _ ty1) ty2) = Just <$> typeSubstitutionTop p ty2 ty1
computeType p (TyVar i _) = do
    n <- get
    x <- isTypeAdd p n i
    if x
    then getTypeAbb p n i
    else return Nothing

computeType _ _ = return Nothing

--let rec simplifyty ctx tyT =
--  let tyT =
--    match tyT with
--        TyApp(tyT1,tyT2) -> TyApp(simplifyty ctx tyT1,tyT2)
--      | tyT -> tyT
--  in
--  try
--    let tyT' = computety ctx tyT in
--    simplifyty ctx tyT'
--  with NoRuleApplies -> tyT

simplifyType :: SourcePos -> Type -> Eval Type
simplifyType p z@(TyApp ty1 ty2) = do
    tyX <- flip(TyApp) ty2 <$> simplifyType p ty1
    n <- computeType p tyX
    case n of
         Just x -> simplifyType p x
         _ -> return tyX

simplifyType p ty = do
    n <- computeType p ty
    case n of
         Just x -> simplifyType p x
         _ -> return ty

--let rec promote ctx t = match t with
--   TyVar(i,_) ->
--     (match getbinding dummyinfo ctx i with
--         TyVarBind(tyT) -> tyT
--       | _ -> raise NoRuleApplies)
-- | TyApp(tyS,tyT) -> TyApp(promote ctx tyS,tyT)
-- | _ -> raise NoRuleApplies

promote :: SourcePos -> Type -> Eval (Maybe Type)
promote p (TyVar i _) = do
    n <- get
    x <- getBinding p n i
    case x of
         Just (TypeVarBind ty) -> return $ Just ty
         _ -> return Nothing
promote p (TyApp tyS tyT) = liftM(flip(TyApp) tyT) <$> promote p tyS
promote _ _ = return Nothing

--let rec lcst ctx tyS =
--  let tyS = simplifyty ctx tyS in
--  try lcst ctx (promote ctx tyS)
--  with NoRuleApplies -> tyS

lcst :: SourcePos -> Type -> Eval Type
lcst p ty = do
    tyS <- simplifyType p ty
    x <- promote p tyS
    case x of
        Just ty -> lcst p ty
        Nothing -> return tyS
