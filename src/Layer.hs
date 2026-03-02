module Layer where

import qualified Data.Bifunctor as Bifunctor (bimap)

type Layer s -- input
           t -- gradient to pass to the layer below
           a -- output
           b -- gradient from the layer above
          = s -> (a, b -> t)

type Layer2 a b = a -> (b, b -> a)

compose2 :: Layer2 b c -> Layer2 a b -> Layer2 a c
compose2 f g x = let (b, f') = g x
                     (c, g') = f b
                  in (c, f' . g')

compose :: Layer a t c b -> Layer s u a t -> Layer s u c b
compose f g x = let (gx, dg) = g x
                    (fgx, df) = f gx
                 in (fgx, dg . df)

-- Split the input into two outputs
-- The derivative is the sum of the output derivatives
dup :: Num a => Layer2 a (a, a)
dup a = ((a,a), uncurry (+))

identity :: Layer2 a a
identity a = (a, id)

-- a-[f]-b
--          ==> (a, c)--[ (f, g) ]--(b, d)
-- c-[g]-d
bimap :: Layer2 a b -> Layer2 c d -> Layer2 (a, c) (b,d)
bimap f g (a, c) = let (b, da) = f a
                       (d, dc) = g c
                    in ((b, d), Bifunctor.bimap da dc)

swap :: Layer2 (a,b) (b,a)
swap (a,b)  = _1
-- split :: Layer (s1, s2) (t1, t2) a b -- the binary operator
--       -> Layer u v s1 t1
--       -> Layer w x s2 t2
--       -> Layer s t a b
-- split = _

merge :: Layer s1 t1 a1 b -> Layer s2 t2 a2 b -> Layer (s1, s2) (t1, t2) (a1, a2) b
merge l1 l2 (x1, x2) = let (a1, bt1) = l1 x1
                           (a2, bt2) = l2 x2
                        in ((a1, a2), \b -> (bt1 b, bt2 b))

constant :: Fractional a => a -> Layer s a a b
constant x _ = (x, const 0)

add :: (Num a, Num b) => Layer (a, a) (b, b) a b
add (x, y) = (x + y, \g -> (g, g))

sub :: (Num a, Num b) => Layer (a, a) (b, b) a b
sub (x, y) = (x - y, \g -> (g, g))

mul :: (Num a) => Layer (a, a) (a, a) a a
mul (x, y) = (x*y, \g -> (x*g, y*g))

-- x^y
-- f = x^y
-- df/dx = yx^(y-1)
-- df/dy = x^y . ln(x)
pow :: Floating a => Layer (a, a) (a, a) a a
pow (x, y) = (x**y, \g -> (y*x**(y-1)*g, x**y * log x * g))

relu :: (Ord a, Num a) => Layer a a a a
relu x = (max 0 x, \g -> if x > 0 then g else 0)
