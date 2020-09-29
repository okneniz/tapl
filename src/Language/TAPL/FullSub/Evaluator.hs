module Language.TAPL.FullSub.Evaluator (evalString) where

import qualified Data.Map.Lazy as Map

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.FullSub.Types
import Language.TAPL.FullSub.Parser
import Language.TAPL.FullSub.TypeChecker
import Language.TAPL.FullSub.Pretty
import Language.TAPL.FullSub.Context

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
        Left e -> Left $ show e
        Right ([], _) -> return ""
        Right (commands, names) -> runExcept (evalStateT (f commands) names)
    where
        f cs = do
            cs' <- evalCommands cs
            if null cs'
            then return ""
            else do let t = last cs'
                    ty <- typeOf t
                    t' <- prettify t
                    ty' <- prettifyType ty
                    return $ show t' ++ ":" ++ show ty'

evalCommands :: [Command] -> Eval AST
evalCommands [] = return []
evalCommands ((Bind _ name binding):cs) = do
   modify $ bind name binding
   evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval ts):cs) = do
    _ <- typeCheck ts
    let ts' = whileJust normalize <$> ts
    cs' <- evalCommands cs
    return $ ts' ++ cs'

typeCheck :: AST -> Eval Type
typeCheck [] = lift $ throwE "attempt to check empty AST"
typeCheck [t] = typeOf t
typeCheck (t:ts) = typeOf t >> typeCheck ts

normalize :: Term -> Maybe Term
--    TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
--      termSubstTop v2 t12
--  | TmApp(fi,v1,t2) when isval ctx v1 ->
--      let t2' = eval1 ctx t2 in
--      TmApp(fi, v1, t2')
--  | TmApp(fi,t1,t2) ->
--      let t1' = eval1 ctx t1 in
--      TmApp(fi, t1', t2)

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = flip (TApp p) t2 <$> normalize t1

--  | TmIf(_,TmTrue(_),t2,t3) ->
--      t2
--  | TmIf(_,TmFalse(_),t2,t3) ->
--      t3
--  | TmIf(fi,t1,t2,t3) ->
--      let t1' = eval1 ctx t1 in
--      TmIf(fi, t1', t2, t3)

normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf p t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf p t1' t2 t3

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

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord p fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields
    return $ TRecord p (Map.fromList fields')
    where evalField (k, v) | isVal v = return (k, v)
          evalField (k, v) = ((,) k) <$> normalize v

--  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
--      (try List.assoc l fields
--       with Not_found -> raise NoRuleApplies)
--  | TmProj(fi, t1, l) ->
--      let t1' = eval1 ctx t1 in
--      TmProj(fi, t1', l)

normalize (TProj _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = Map.lookup key fields
normalize (TProj p t@(TRecord _ _) (TKeyword x key)) = flip(TProj p) (TKeyword x key) <$> normalize t
normalize (TProj p t k) = flip(TProj p) k <$> normalize t

--  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
--      termSubstTop v1 t2
--  | TmLet(fi,x,t1,t2) ->
--      let t1' = eval1 ctx t1 in
--      TmLet(fi, x, t1', t2)

normalize (TLet _ _ t1 t2) | isVal t1 = return $ termSubstitutionTop t1 t2
normalize (TLet p v t1 t2) = flip (TLet p v) t2 <$> normalize t1

--  | TmFix(fi,v1) as t when isval ctx v1 ->
--      (match v1 with
--         TmAbs(_,_,_,t12) -> termSubstTop t t12
--       | _ -> raise NoRuleApplies)
--  | TmFix(fi,t1) ->
--      let t1' = eval1 ctx t1
--      in TmFix(fi,t1')

normalize t1@(TFix _ v@(TAbs _ _ _ t2)) | isVal v = return $ termSubstitutionTop t1 t2
normalize (TFix p t) = TFix p <$> normalize t

--  | TmAscribe(fi,v1,tyT) when isval ctx v1 ->
--      v1
--  | TmAscribe(fi,t1,tyT) ->
--      let t1' = eval1 ctx t1 in
--      TmAscribe(fi,t1',tyT)

normalize (TAscribe _ t _) = return t

--  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
--      TmFloat(fi, f1 *. f2)
--  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
--      let t2' = eval1 ctx t2 in
--      TmTimesfloat(fi,t1,t2')
--  | TmTimesfloat(fi,t1,t2) ->
--      let t1' = eval1 ctx t1 in
--      TmTimesfloat(fi,t1',t2)

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) = return $ TFloat p (t1 * t2)
normalize (TTimesFloat p t1 t2) | isVal t1 = TTimesFloat p t1 <$> normalize t2
normalize (TTimesFloat p t1 t2) = flip(TTimesFloat p) t2 <$> normalize t1

--  | TmSucc(fi,t1) ->
--      let t1' = eval1 ctx t1 in
--      TmSucc(fi, t1')

normalize (TSucc p t) = TSucc p <$> normalize t

--  | TmPred(_,TmZero(_)) ->
--      TmZero(dummyinfo)
--  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
--      nv1
--  | TmPred(fi,t1) ->
--      let t1' = eval1 ctx t1 in
--      TmPred(fi, t1')

normalize (TPred _ (TZero p)) = return $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred p t) = TPred p <$> normalize t

--  | TmIsZero(_,TmZero(_)) ->
--      TmTrue(dummyinfo)
--  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
--      TmFalse(dummyinfo)
--  | TmIsZero(fi,t1) ->
--      let t1' = eval1 ctx t1 in
--      TmIsZero(fi, t1')

normalize (TIsZero _ (TZero p)) = return $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = return $ TFalse p
normalize (TIsZero p t) = TIsZero p <$> normalize t

normalize _ = Nothing
