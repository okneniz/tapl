module Language.TAPL.Common.Helpers where

import Text.Parsec (Parsec, try, oneOf, many, spaces, optional, getState, putState, modifyState)
import Control.Monad (unless)
import Control.Monad.Trans.State.Lazy
import Control.Applicative ((<|>), Alternative)

whileM :: (Monad m, Alternative m) => (a -> m a) -> a -> m a
whileM f x = (whileM f =<< f x) <|> return x

ucid :: Parsec String a String
ucid = (:) <$> (try x) <*> try (many y)
    where x = oneOf ['A'..'Z']
          y = oneOf $ ['A'..'Z'] <> ['a'..'z']

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = flip(unless) s =<< p

withTmpStateT :: Monad m => (s -> s) -> StateT s m b -> StateT s m b
withTmpStateT f g = do
    s <- get
    x <- modify f *> g
    put s
    return x

withState :: (u -> u) -> Parsec String u a -> Parsec String u a
withState f g = do
    s <- getState
    x <- modifyState f *> g
    putState s
    return x

padded :: Parsec String u a -> Parsec String u a
padded x = optional spaces *> x <* optional spaces

nvm :: Monad m => m (Maybe a)
nvm = return Nothing

ok :: (Monad m1, Monad m2) => a -> m1 (m2 a)
ok = return.return