{-# LANGUAGE RankNTypes #-}
module CoYoneda where

toCoYoneda :: (a -> b) -> (forall x . (x -> a) -> (x -> b))
toCoYoneda f g = f . g

fromCoYoneda :: (forall x . ((x -> a) -> (x -> b))) -> (a -> b)
fromCoYoneda nt  = nt id
