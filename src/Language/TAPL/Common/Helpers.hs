module Language.TAPL.Common.Helpers where

import Text.Parsec (Parsec, try, oneOf, many, spaces, optional)
import Control.Monad (unless)
import Control.Monad.Trans.State.Lazy

whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = case f x of
                     Just x' -> whileJust f x'
                     Nothing -> x

ucid :: Parsec String a String
ucid = (:) <$> (try x) <*> (try $ many y)
    where x = oneOf ['A'..'Z']
          y = oneOf $ ['A'..'Z'] <> ['a'..'z']

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = flip(unless) s =<< p

withTmpStateT :: Monad m => (s -> s) -> StateT s m b -> StateT s m b
withTmpStateT f g = do
    s <- get
    x <- modify f >> g
    put s
    return x

padded :: Parsec String u a -> Parsec String u a
padded x = optional spaces *> x <* optional spaces
