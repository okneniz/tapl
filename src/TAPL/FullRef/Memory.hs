{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullRef.Memory where

import TAPL.FullRef.Types

data LCMemory = LCMemory [Term]

class Memory m where
    extend :: m -> Term -> (Int, m)
    lookup :: m -> Location -> Term
    update :: m -> Location -> Term -> m
    size :: m -> Int

instance Memory LCMemory where
    extend (LCMemory s) t = (length s, LCMemory (s ++ [t]))
    lookup (LCMemory s) l = s !! l
    update (LCMemory s) l t =
        let f 0 (_:rest) = t:rest
            f location (t':rest) = t':(f (location - 1) rest)
            f _ _ = error "updateStore: bad pointer"
        in LCMemory $ f l s
    size (LCMemory s) = length s

instance Show LCMemory where
    show (LCMemory x) = "<memory: " ++ show x ++ " >"
