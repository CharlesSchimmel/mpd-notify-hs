
module Utilities 
  (
  liftTuple
, maybeWrap
  )where

import Control.Applicative

liftTuple :: Applicative f => (f a, f b) -> f (a,b)
liftTuple (x,y) = liftA2 (,) x y

maybeWrap :: Either a b -> Maybe b
maybeWrap (Right b) = Just b
maybeWrap _ = Nothing
