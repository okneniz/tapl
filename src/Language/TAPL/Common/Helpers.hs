module Language.TAPL.Common.Helpers where

import Text.Parsec

whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = case f x of
                     Just x' -> whileJust f x'
                     Nothing -> x

ucid :: Parsec String a String
ucid = (:) <$> (try $ oneOf ['A'..'Z'])
           <*> (try $ many $ oneOf ['a'..'z'])
