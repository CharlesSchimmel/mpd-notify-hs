{-# LANGUAGE ScopedTypeVariables #-}
module Images
    (
    getCommonColor
  , findDimensions
  , Art(..)
  , Dimensions(..)
  , ColorRGB(..)
    ) where 


import Codec.Picture
import qualified Codec.Picture.Metadata as Meta
import Data.Int
import Data.List
import Data.Tuple (swap)
import Vision.Histogram hiding (map)
import Vision.Image hiding (map)
import Vision.Image.JuicyPixels
import Vision.Primitive

type Dimensions = (Integer, Integer)

data Art = Art { filepath :: FilePath , dimensions :: Dimensions }
  deriving (Show)

instance Eq Art where
  (Art a c) == (Art b d) = a == b && c == d

instance Ord Art where
  (Art _ a) `compare` (Art _ b) = a `compare` b

type ColorRGB = (Int,Int,Int)

-- Given a valid file, will return a triple of 256 RGB values (R,G,B)
getCommonColor :: FilePath -> IO ColorRGB
getCommonColor image = do
    foo <- readImage image
    let io1 = (toFridayRGB . convertRGB8) <$> foo
    case io1 of
      (Right (rgb1 :: RGB)) -> do
          -- generalize to 16 bit color. using anything higher gets too specific
          let histo = histogram (Just $ ix3 16 16 16) rgb1 :: Histogram DIM3 Int32
              -- pulls the tuple of (color frequency,(r,g,b,)) (aka assocs) from histo
              -- swap them so we can sort on the frequency, then take the highest
              mostCommon = rgbVals $ last $ sort $ map swap $ assocs histo
              -- foo = to256 mostCommon
          return $ to256 mostCommon
                    -- pull values from DIM3 and convert to 256 depth. 
                    -- take the average of the bins. Use arithmetic-foo to keep a pure Integer type
              where to256 (x,y,z) = (8*(2*x+1),8*(2*y+1),8*(2*z+1))
                    rgbVals (_, Z :. x :. y :. z) = (x,y,z)
      _ -> return (0,0,0)

findDimensions :: FilePath -> IO (Maybe Art)
findDimensions imagePath = do
    image <- readImageWithMetadata imagePath
    case image of
      (Right foo) -> do
          return $ (Art imagePath) <$> (eitherIt $ pullDimns foo)
             where pullDimns (_,meta) = (toInteger <$> Meta.lookup Meta.Width meta, toInteger <$> Meta.lookup Meta.Height meta)
                   -- There HAS to be an applicative way of doing this
                   eitherIt (Just x, Just y) = Just (x,y)
                   eitherIt _ = Nothing
      _ -> return Nothing
