module Util (
    mix 
    , remix
    , dup
    , dupFirst
) where

-- a collection of random functions that don't really fit into any other modules

mix :: ((a,b),c) -> (a,(b,c))
mix ((a,b),c) = (a,(b,c))

remix :: (a,(b,c)) -> ((a,b),c)
remix (a,(b,c)) = ((a,b),c)

dup :: b -> (b, b)
dup x = (x,x)

dupFirst :: (a,b) -> (a,(a,b))
dupFirst (a,b) = (a,(a,b))