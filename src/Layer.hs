module Layer where

import qualified Data.Bifunctor as Bifunctor (bimap)
import qualified Data.Tuple as Tuple (swap)

newtype Layer a b = Layer { runLayer :: a -> (b, b -> a) }

compose :: Layer b c -> Layer a b -> Layer a c
compose f g = Layer $ \x -> let (b, f') = runLayer g x
                                (c, g') = runLayer f b
                               in (c, f' . g')

-- Split the input into two outputs
-- The derivative is the sum of the output derivatives
dup :: Num a => Layer a (a, a)
dup = Layer $ \a -> ((a,a), uncurry (+))

identity :: Layer a a
identity = Layer $ \a -> (a, id)

-- a-[f]-b      a--|   f    |--b
--          ==>    |        |
-- c-[g]-d      c--|   g    |--d
bimap :: Layer a b -> Layer c d -> Layer (a, c) (b,d)
bimap f g = Layer $ \(a, c) -> let (b, da) = runLayer f a
                                   (d, dc) = runLayer g c
                                in ((b, d), Bifunctor.bimap da dc)

-- a--| |--b
-- b--| |--a
swap :: Layer (a,b) (b,a)
swap = Layer $ \x -> (Tuple.swap x, Tuple.swap)

-- a--[]
terminate :: Layer a ()
terminate = Layer $ \a -> ((), const a)

collapse :: Layer ((), ()) a -> Layer () a
collapse f = Layer $ \_ -> let (a, _) = runLayer f ((), ()) in (a, const ())

constant :: Fractional a => a -> Layer () a
constant x = Layer $ const (x, const ())

add :: Num a => Layer (a, a) a
add = Layer $ \(x, y) -> (x + y, \g -> (g, g))

sub :: Num a => Layer (a, a) a
sub = Layer $ \(x, y) -> (x - y, \g -> (g, g))

mul :: Num a => Layer (a, a) a
mul = Layer $ \(x, y) -> (x*y, \g -> (x*g, y*g))

-- x^y
-- f = x^y
-- df/dx = yx^(y-1)
-- df/dy = x^y . ln(x)
pow :: Floating a => Layer (a, a) a
pow = Layer $ \(x, y) -> (x**y, \g -> (y*x**(y-1)*g, x**y * log x * g))

relu :: (Ord a, Num a) => Layer a a
relu = Layer $ \x -> (max 0 x, \g -> if x > 0 then g else 0)

neg :: Num a => Layer a a
neg = Layer $ \x -> (negate x, negate)

example = let a = constant (-4)
              b = constant 2
              c = compose add (bimap a b)
              d1 = compose mul (bimap a b)
              d2 = compose pow (bimap b (constant 3))
              d = compose add (bimap d1 d2)
              c' = compose add (bimap c (constant 1))
              c''1 = compose add (bimap (constant 1) c')
              c''2 = compose add (bimap c''1 (compose neg a))
              d' = _1
           in undefined
