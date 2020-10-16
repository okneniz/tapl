module Language.TAPL.Common.Helpers where

import Text.Parsec (Parsec, try, oneOf, many)
import Control.Monad (unless)
import Control.Monad.Trans.State.Lazy

whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = case f x of
                     Just x' -> whileJust f x'
                     Nothing -> x

ucid :: Parsec String a String
ucid = (:) <$> (try x) <*> (try $ many y)
    where x = oneOf ['A'..'Z']
          y = oneOf $ ['A'..'Z'] ++ ['a'..'z']

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = do
    x <- p
    unless x s

withTmpStateT :: Monad m => (s -> s) -> StateT s m b -> StateT s m b
withTmpStateT f g = do
    s <- get
    modify f
    x <- g
    put s
    return x

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(g <=< f) a = f a >>= g
