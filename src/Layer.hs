{-# LANGUAGE GADTs #-}
module Layer where

import Prelude hiding (div)
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

withConstant :: c -> Layer a (a, c)
withConstant c = Layer $ \a -> ((a, c), fst)

add :: Num a => Layer (a, a) a
add = Layer $ \(x, y) -> (x + y, \g -> (g, g))

sub :: Num a => Layer (a, a) a
sub = Layer $ \(x, y) -> (x - y, \g -> (g, -g))

mul :: Num a => Layer (a, a) a
mul = Layer $ \(x, y) -> (x*y, \g -> (y*g, x*g))

-- f = x/y = xy^-1
-- df/dx = y^-1
-- df/dy = -xy^-2
div :: (Num a, Floating a) => Layer (a, a) a
div = Layer $ \(x, y) -> (x / y, \g -> (g/y, g * (-x)/y**2))

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

mulBy3 :: Num a => Layer a a
mulBy3 = compose mul (withConstant 3)

-- de bruijn indexed environment
data Idx env where
  Z :: Num rest => Idx (Double, rest)
  S :: Num rest => Idx rest -> Idx (Double, rest)

-- A language for layer computations
data Expr env where
  Var :: Idx env -> Expr env
  Const :: Double -> Expr env
  Add :: Expr env -> Expr env -> Expr env
  Sub :: Expr env -> Expr env -> Expr env
  Mul :: Expr env -> Expr env -> Expr env
  Div :: Expr env -> Expr env -> Expr env
  Pow :: Expr env -> Expr env -> Expr env
  Relu :: Expr env -> Expr env

compile :: Num env => Expr env -> Layer env Double
compile (Var i) = compileIdx i
compile (Const a) = Layer $ const (a, const 0)
compile (Add x y) = compose add $ compose (bimap (compile x) (compile y)) dup
compile (Sub x y) = compose sub $ compose (bimap (compile x) (compile y)) dup
compile (Mul x y) = compose mul $ compose (bimap (compile x) (compile y)) dup
compile (Div x y) = compose div $ compose (bimap (compile x) (compile y)) dup
compile (Pow x y) = compose pow $ compose (bimap (compile x) (compile y)) dup
compile (Relu x) = compose relu $ compile x

compileIdx :: Idx env -> Layer env Double
compileIdx Z = fst'
compileIdx (S i) = compose (compileIdx i) snd'

-- A layer which selects the first input
-- The second input isn't used, so its gradient is set to 0
fst' :: Num b => Layer (a, b) a
fst' = Layer $ \(a, _) -> (a, \g -> (g, 0))

-- A layer which selects the second input
snd' :: Num a => Layer (a, b) b
snd' = Layer $ \(_, b) -> (b, \g -> (0, g))



-- an orphan instance required for our environment (a nested tuple of Double) to be Num.
-- we use this e.g. in fst' where we set the gradient to 0
instance (Num a, Num b) => Num (a, b) where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) * (c, d) = (a * c, b * d)
  fromInteger n = (fromInteger n, fromInteger n)
  negate (a, b) = (negate a, negate b)
  abs (a, b) = (abs a, abs b)
  signum (a, b) = (signum a, signum b)

example :: Expr (Double, (Double, Double)) -- (a, b, not-used)
example =
  let a = Var Z
      b = Var $ S Z
      c = Add a b
      d = Add (Mul a b) (Pow b (Const 3))
      c' = Add c $ Add c (Const 1)
      c'' = Add c' $ Add (Const 1) $ Add c' (Sub (Const 0) a)
      d' = Add d $ Add (Mul d (Const 2)) (Relu (Add b a))
      d'' = Add d' $ Add (Mul (Const 3) d') (Relu (Sub b a))
      e = Sub c'' d''
      f = Pow e (Const 2)
      g = Div f (Const 2)
      g' = Add g $ Div (Const 10) f
   in
      g'

result = let layer = compile example
          in runLayer layer (-4, (2, -10000))

graph :: Expr env -> String
graph = unlines . go ""
  where
    go indent (Var i) = [indent ++ "Var " ++ show (idxToInt i)]
    go indent (Const a) = [indent ++ "Const " ++ show a]
    go indent (Relu x) =
      (indent ++ "Relu") : go (indent ++ "  ") x
    go indent (Add x y) = binOp indent "Add" x y
    go indent (Sub x y) = binOp indent "Sub" x y
    go indent (Mul x y) = binOp indent "Mul" x y
    go indent (Div x y) = binOp indent "Div" x y
    go indent (Pow x y) = binOp indent "Pow" x y

    binOp indent name x y =
      (indent ++ name)
        : go (indent ++ "├ ") x
       ++ go (indent ++ "└ ") y

    idxToInt :: Idx env -> Int
    idxToInt Z = 0
    idxToInt (S i) = 1 + idxToInt i
