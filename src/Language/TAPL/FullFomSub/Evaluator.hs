module Language.TAPL.FullFomSub.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad (liftM, liftM2, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Text.Parsec (SourcePos)

import Language.TAPL.FullFomSub.Types
import Language.TAPL.FullFomSub.Parser
import Language.TAPL.FullFomSub.Context
import Language.TAPL.FullFomSub.TypeChecker
import Language.TAPL.FullFomSub.Pretty
import Language.TAPL.Common.Context (bind)

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, names) -> runExcept $ evalStateT (f commands) []
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Command] -> Eval [String]
evalCommands [] = return []

evalCommands ((Bind p name b):cs) = do
    sb <- checkBinding p b
    modify $ bind name sb
    evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval (t:ts)):cs) = do
    ty <- typeOf t
    t' <- fullNormalize t
    (:) <$> render t' ty <*> evalCommands ((Eval ts):cs)

fullNormalize :: Term -> Eval Term
fullNormalize t = do
    t' <- normalize t
    case t' of
         Just x -> fullNormalize x
         _ -> return t

checkBinding :: SourcePos -> Binding -> Eval Binding
checkBinding p (TypeAddBind ty Nothing) = (\k -> TypeAddBind ty (Just k)) <$> kindOf p ty
checkBinding p b@(TypeAddBind ty (Just k1)) = do
    k2 <- kindOf p ty
    unless (k1 == k2) (lift $ throwE $ "Kind of binding does not match declared kind")
    return b

checkBinding _ x = return x

continue :: (Monad m1, Monad m2) => a -> m1 (m2 a)
continue x = (return.return) x

nvm :: Eval (Maybe Term)
nvm = return Nothing

normalize :: Term -> Eval (Maybe Term)
--    TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
--      termSubstTop v2 t12
--  | TmApp(fi,v1,t2) when isval ctx v1 ->
--      let t2' = eval1 ctx t2 in
--      TmApp(fi, v1, t2')
--  | TmApp(fi,t1,t2) ->
--      let t1' = eval1 ctx t1 in
--      TmApp(fi, t1', t2)
normalize (TApp p (TAbs _ _ tyT11 t12) v2) | isVal v2 = Just <$> termSubstitutionTop p v2 t12
normalize (TApp p v1 t2) | isVal v1 = liftM(TApp p v1) <$> normalize t2
normalize (TApp p t1 t2) = liftM(flip (TApp p) t2) <$> normalize t1

--  | TmIf(_,TmTrue(_),t2,t3) ->
--      t2
--  | TmIf(_,TmFalse(_),t2,t3) ->
--      t3
--  | TmIf(fi,t1,t2,t3) ->
--      let t1' = eval1 ctx t1 in
--      TmIf(fi, t1', t2, t3)

normalize (TIf _ (TTrue _) t _ ) = continue t
normalize (TIf _ (TFalse _) _ t) = continue t
normalize (TIf p t1 t2 t3) = liftM(\t1' -> TIf p t1' t2 t3) <$> normalize t1

--  | TmRecord(fi,fields) ->
--      let rec evalafield l = match l with
--        [] -> raise NoRuleApplies
--      | (l,vi)::rest when isval ctx vi ->
--          let rest' = evalafield rest in
--          (l,vi)::rest'
--      | (l,ti)::rest ->
--          let ti' = eval1 ctx ti in
--          (l, ti')::rest
--      in let fields' = evalafield fields in
--      TmRecord(fi, fields')

normalize (TRecord _ fields) | (Map.size fields) == 0 = nvm
normalize t@(TRecord _ _) | isVal t = nvm
normalize (TRecord p fs) = do
    fs' <- mapM evalField $ Map.toList fs
    continue $ TRecord p (Map.fromList fs')
    where evalField (k,v) = (,) k <$> fullNormalize v

--  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
--      (try List.assoc l fields
--       with Not_found -> raise NoRuleApplies)
--  | TmProj(fi, t1, l) ->
--      let t1' = eval1 ctx t1 in
--      TmProj(fi, t1', l)

normalize (TProj _ t@(TRecord _ fs) (TKeyword _ k)) | isVal t = return $ Map.lookup k fs
normalize (TProj p t@(TRecord _ _) (TKeyword x k)) = liftM(\t' -> TProj p t' (TKeyword x k)) <$> normalize t

--  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
--      termSubstTop v1 t2
--  | TmLet(fi,x,t1,t2) ->
--      let t1' = eval1 ctx t1 in
--      TmLet(fi, x, t1', t2)

normalize (TLet p _ t1 t2) | isVal t1 = Just <$> termSubstitutionTop p t1 t2
normalize (TLet p v t1 t2) = liftM(flip(TLet p v) t2) <$> normalize t1

--  | TmFix(fi,v1) as t when isval ctx v1 ->
--      (match v1 with
--         TmAbs(_,_,_,t12) -> termSubstTop t t12
--       | _ -> raise NoRuleApplies)
--  | TmFix(fi,t1) ->
--      let t1' = eval1 ctx t1
--      in TmFix(fi,t1')

normalize t1@(TFix p a@(TAbs _ _ _ t2)) | isVal a = Just <$> termSubstitutionTop p t1 t2
normalize (TFix p t) = liftM(TFix p) <$> normalize t

--  | TmAscribe(fi,v1,tyT) when isval ctx v1 ->
--      v1
--  | TmAscribe(fi,t1,tyT) ->
--      let t1' = eval1 ctx t1 in
--      TmAscribe(fi,t1',tyT)

normalize (TAscribe _ t _) | isVal t = continue t
normalize (TAscribe x t ty) = liftM(flip(TAscribe x) ty) <$> normalize t

--  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
--      TmFloat(fi, f1 *. f2)
--  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
--      let t2' = eval1 ctx t2 in
--      TmTimesfloat(fi,t1,t2')
--  | TmTimesfloat(fi,t1,t2) ->
--      let t1' = eval1 ctx t1 in
--      TmTimesfloat(fi,t1',t2)

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) = continue $ TFloat p (t1 * t2)
normalize (TTimesFloat p t1 t2) | isVal t1 = liftM(TTimesFloat p t1) <$> normalize t2
normalize (TTimesFloat p t1 t2) = liftM(flip (TTimesFloat p) t2) <$> normalize t1

--  | TmTApp(fi,TmTAbs(_,x,_,t11),tyT2) ->
--      tytermSubstTop tyT2 t11
--  | TmTApp(fi,t1,tyT2) ->
--      let t1' = eval1 ctx t1 in
--      TmTApp(fi, t1', tyT2)

normalize (TTApp p (TTAbs _ _ _ t11) tyT2) = Just <$> typeTermSubstitutionTop p tyT2 t11
normalize (TTApp p t1 tyT2) = liftM(flip(TTApp p) tyT2) <$> normalize t1

--  | TmSucc(fi,t1) ->
--      let t1' = eval1 ctx t1 in
--      TmSucc(fi, t1')
normalize (TSucc p t) = liftM(TSucc p) <$> normalize t

--  | TmPred(_,TmZero(_)) ->
--      TmZero(dummyinfo)
--  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
--      nv1
--  | TmPred(fi,t1) ->
--      let t1' = eval1 ctx t1 in
--      TmPred(fi, t1')

normalize (TPred _ (TZero p)) = continue $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = continue t
normalize (TPred p t) = liftM(TPred p) <$> normalize t

--  | TmIsZero(_,TmZero(_)) ->
--      TmTrue(dummyinfo)
--  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
--      TmFalse(dummyinfo)
--  | TmIsZero(fi,t1) ->
--      let t1' = eval1 ctx t1 in
--      TmIsZero(fi, t1')

normalize (TIsZero _ (TZero p)) = continue $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = continue $ TFalse p
normalize (TIsZero p t) = liftM(TIsZero p) <$> normalize t

--  | TmUnpack(fi,_,_,TmPack(_,tyT11,v12,_),t2) when isval ctx v12 ->
--      tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
--  | TmUnpack(fi,tyX,x,t1,t2) ->
--      let t1' = eval1 ctx t1 in
--      TmUnpack(fi,tyX,x,t1',t2)

normalize (TUnpack p1 _ _ (TPack p2 tyT11 v12 _) t2) | isVal v12 = do
    x <- termShift p2 1 v12
    y <- termSubstitutionTop p2 x t2
    Just <$> typeTermSubstitutionTop p2 tyT11 y

normalize (TUnpack p ty x t1 t2) = liftM(flip(TUnpack p ty x) t2) <$> normalize t1

--  | TmPack(fi,tyT1,t2,tyT3) ->
--      let t2' = eval1 ctx t2 in
--      TmPack(fi,tyT1,t2',tyT3)
normalize (TPack p ty1 t2 ty3) = liftM(flip(TPack p ty1) ty3) <$> normalize t2
normalize _ = nvm
