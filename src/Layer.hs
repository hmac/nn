module Layer where

type Layer s -- input
           t -- gradient to pass to the layer below
           a -- output
           b -- gradient from the layer above
          = s -> (a, b -> t)

compose :: Layer a t c b -> Layer s u a t -> Layer s u c b
compose f g x = let (gx, dg) = g x
                    (fgx, df) = f gx
                 in (fgx, dg . df)

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
