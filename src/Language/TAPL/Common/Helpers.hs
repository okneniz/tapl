module Language.TAPL.Common.Helpers where

whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = case f x of
                     Just x' -> whileJust f x'
                     Nothing -> x