module Layer where

import qualified Data.Bifunctor as Bifunctor (bimap)
import qualified Data.Tuple as Tuple (swap)

type Layer a b = a -> (b, b -> a)

compose :: Layer b c -> Layer a b -> Layer a c
compose f g x = let (b, f') = g x
                    (c, g') = f b
                  in (c, f' . g')

-- Split the input into two outputs
-- The derivative is the sum of the output derivatives
dup :: Num a => Layer a (a, a)
dup a = ((a,a), uncurry (+))

identity :: Layer a a
identity a = (a, id)

-- a-[f]-b      a--|   f    |--b
--          ==>    |        |
-- c-[g]-d      c--|   g    |--d
bimap :: Layer a b -> Layer c d -> Layer (a, c) (b,d)
bimap f g (a, c) = let (b, da) = f a
                       (d, dc) = g c
                    in ((b, d), Bifunctor.bimap da dc)

-- a--| |--b
-- b--| |--a
swap :: Layer (a,b) (b,a)
swap x  = (Tuple.swap x, Tuple.swap)

-- a--[]
terminate :: Layer a ()
terminate a = ((), const a)

collapse :: Layer ((), ()) a -> Layer () a
collapse f _ = let (a, _) = f ((), ()) in (a, const ())

constant :: Fractional a => a -> Layer () a
constant x _ = (x, const ())

add :: Num a => Layer (a, a) a
add (x, y) = (x + y, \g -> (g, g))

sub :: Num a => Layer (a, a) a
sub (x, y) = (x - y, \g -> (g, g))

mul :: Num a => Layer (a, a) a
mul (x, y) = (x*y, \g -> (x*g, y*g))

-- x^y
-- f = x^y
-- df/dx = yx^(y-1)
-- df/dy = x^y . ln(x)
pow :: Floating a => Layer (a, a) a
pow (x, y) = (x**y, \g -> (y*x**(y-1)*g, x**y * log x * g))

relu :: (Ord a, Num a) => Layer a a
relu x = (max 0 x, \g -> if x > 0 then g else 0)

neg :: Num a => Layer a a
neg x = (negate x, negate)

-- can't do Num instance because Layer is a type alias?
-- instance (Num a, Num b) => Num (Layer a b) where
