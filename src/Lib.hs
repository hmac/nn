{-# LANGUAGE RankNTypes #-}
module Lib where

import Data.Profunctor (Profunctor(..), Strong(..), Choice(..), Forget(..))
import Data.Profunctor.Traversing (Traversing(..))

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- data Value a = Value {value :: a, grad :: a}
--     deriving (Eq, Ord, Show)

-- fromInt :: Int -> Value Int
-- fromInt i = Value { value = i, grad = 0}

-- instance Num a => Num (Value a) where
--     -- d/dx (a + b) = da/dx + db/dx
--     v1 + v2 = Value { value = value v1 + value v2, grad = grad v1 + grad v2 }
--     -- d/dx ab = da/dx b + db/dx a
--     v1 * v2 = Value { value = value v1 * value v2, grad = grad v1 * value v2 + grad v2 * value v1}
--     abs v = Value { value = abs (value v), grad = grad v}
--     signum v = Value { value = signum (value v), grad = grad v}
--     fromInteger n = Value { value = fromInteger n, grad = 0 }
--     negate v = Value { value = negate (value v), grad = grad v}


-- profunctor lenses
type Optic p s t a b = p a b -> p s t
type Lens s t a b = forall p. Strong p => Optic p s t a b
type Prism s t a b = forall p. Choice p => Optic p s t a b
type Setter s t a b = Optic (->) s t a b
type Traversal s t a b = forall p. Traversing p => Optic p s t a b
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
type Fold p s t a b = Optic (Forget p) s t a b
type Getter s t a b = forall p. Fold p s t a b

strong :: Strong p => (a -> b -> c) -> p a b -> p a c
strong f pab = dimap (\a -> (a,a)) (\(b,a) -> f a b) $ first' pab

uncurry' :: Strong p => p a (b->c) -> p (a,b) c
uncurry' = rmap (\(f, x) -> f x) . first'

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set_ = strong set_ . lmap get

view :: Fold a s t a b -> s -> a
view f = runForget (f (Forget id))

set :: Setter s t a b -> s -> b -> t
set p s b = p (const b) s

to :: (s -> a) -> Getter s t a b
to sa = Forget . lmap sa . runForget


-- A node in a micrograd graph is a pair of functions:
-- 1. a function from input to output
-- 2. a function from a change in input to a change in output (gradient)

-- Lens s - the input
--      t - the gradient to pass to the layer below, dL/dinput
--      a - the output
--      b - the gradient from the layer above, dL/doutput

-- constant values have 0 gradient
-- constant :: Fractional a => a -> Lens s a a b
-- constant x = lens (const x) (\_ _ -> 0.0)

-- adding two layers
-- the gradient d/dx(A + B) = dA/dx + dB/dx
-- add :: (Num a, Num t) => Lens s t a b -> Lens s t a b -> Lens s t a b
-- add n1 n2 = lens (\i -> view n1 i + view n2 i) (\i g -> set n1 i g + set n2 i g)

-- A, B
-- d/dA (A + B) =
-- add :: (Num a, Num b) => Lens (a, a) (b, b) a b
-- add = lens (uncurry (+)) (\_ g -> (g, g))

-- sub :: (Num a, Num b) => Lens (a, a) (b, b) a b
-- sub = lens (uncurry (-)) (\_ g -> (g, g))

-- mul :: Num a => Lens (a, a) (a, a) a a
-- mul = lens (uncurry (*)) (\(i1, i2) g -> (i1*g, i2*g))

-- f = A^B
-- df/dA = BA^(B-1)
-- df/dB = A^B . ln(A)
-- pow :: Floating a => Lens (a, a) (a, a) a a
-- pow = lens (uncurry (**)) $ \(iA, iB) g -> let ddA = iB * iA ** (iB - 1)
--                                                ddB = iA ** iB * log iA
--                                             in (ddA * g, ddB * g)

-- relu :: (Ord a, Num a) => Lens a a a a
-- relu = lens (max 0) (\i g -> if i > 0 then g else 0)



--  d/dx(AB) = AdB/dx + BdA/dx
-- mul :: (Num a) => Lens s a a b -> Lens s a a b -> Lens s a a b
-- mul n1 n2 = lens (\i -> view n1 i * view n2 i) (\i g -> set n1 i g * view n2 i + set n2 i g * view n1 i )

-- d/dx(A^B) = BA^(B-1)
-- pow :: Floating a => Lens s a a b -> Lens s a a b -> Lens s a a b
-- pow n1 n2 = lens (\i -> view n1 i ** view n2 i) (\i g -> let b = view n2 i
--                                                              a = view n1 i
--                                                           in b * a ** (b-1))

-- instance Fractional a => Fractional (Value a) where
--   -- fromRational :: Fractional a => Rational -> Value a
--   fromRational = _
--   -- recip :: Fractional a => Value a -> Value a
--   recip = _

-- instance Floating a => Floating (Value a) where
--   -- pi :: Floating a => Value a
--   pi = _
--   -- exp :: Floating a => Value a -> Value a
--   exp = _
--   -- log :: Floating a => Value a -> Value a
--   log = _
--   -- sin :: Floating a => Value a -> Value a
--   sin = _
--   -- cos :: Floating a => Value a -> Value a
--   cos = _
--   -- asin :: Floating a => Value a -> Value a
--   asin = _
--   -- acos :: Floating a => Value a -> Value a
--   acos = _
--   -- atan :: Floating a => Value a -> Value a
--   atan = _
--   -- sinh :: Floating a => Value a -> Value a
--   sinh = _
--   -- cosh :: Floating a => Value a -> Value a
--   cosh = _
--   -- asinh :: Floating a => Value a -> Value a
--   asinh = _
--   -- acosh :: Floating a => Value a -> Value a
--   acosh = _
--   -- atanh :: Floating a => Value a -> Value a
--   atanh = _
