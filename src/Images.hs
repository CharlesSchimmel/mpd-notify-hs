{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Images
    (
    ColorRGB(..)
  , Cover(..)
  , Dimensions(..)
  , getCommonColor
  , findDimensions
  , loadImage
  , matteImage
    ) where 


import Codec.Picture
import qualified Codec.Picture.Metadata as Meta
import Control.Applicative
import Control.Exception
import Data.Int
import Data.List
import Data.Tuple (swap)
import Debug.Trace
import Vision.Histogram hiding (map)
import Vision.Image hiding (map,Image)
import Vision.Image.JuicyPixels
import Vision.Primitive

type Dimensions = (Int, Int)
data Dim a b where
  Dim :: Integral (a,b) => a -> b -> Dim a b
  -- deriving (Show)

type ColorRGB = (Int,Int,Int)

data Cover = Cover { coverImg :: DynamicImage, coverDimensions :: Dimensions, coverPath :: FilePath }

instance Eq Cover where
  (Cover _ d _) == (Cover _ d' _ ) = d == d'

instance Ord Cover where
  (Cover _ d _) `compare` (Cover _ d' _ ) = d `compare` d'

-- TODO look into using JuicyPixel's Palletize
-- generalize to 16 bit color. using anything higher gets too specific
-- pulls the tuple of (color frequency,(r,g,b,)) (aka assocs) from histo
-- swap them so we can sort on the frequency, then take the highest
-- pull values from DIM3 and convert to 256 depth. 
-- take the average of the bins. Use arithmetic-foo to keep a pure Integer type
getCommonColor :: DynamicImage -> ColorRGB
getCommonColor image = to256 mostCommon
    where rgb1 = (toFridayRGB . convertRGB8) image
          histo = histogram (Just $ ix3 16 16 16) rgb1 :: Histogram DIM3 Int32
          rgbVals (_, Z :. x :. y :. z) = (x,y,z)
          mostCommon = rgbVals $ maximum $ map swap $ assocs histo
          to256 (x,y,z) = (8*(2*x+1),8*(2*y+1),8*(2*z+1))

toPixel8 :: ColorRGB -> PixelRGB8
toPixel8 (x,y,z) = PixelRGB8 (fromIntegral x) (fromIntegral y) (fromIntegral z)

findDimensions :: (DynamicImage, Meta.Metadatas) -> Maybe Dimensions
findDimensions (_,meta) = liftTuple (fromIntegral <$> Meta.lookup Meta.Width meta, fromIntegral <$> Meta.lookup Meta.Height meta)

liftTuple :: Applicative f => (f a, f b) -> f (a,b)
liftTuple (x,y) = liftA2 (,) x y

-- Given a larger dimensions, find the offset needed to center
offset :: (Int,Int) -> (Int,Int) -> (Int,Int)
offset (x,y) (x',y') = ((x-x') `div` 2,(y-y') `div` 2)

-- Given (DynamicImage, Dimensions) and maximum dimensions, try to matte the image against a common colored background
-- if image is larger than maximum dimensions essentially crops it to maximum dimensions
matteImage :: (DynamicImage, Dimensions) -> (Int,Int) -> Image PixelRGB8
matteImage (img,(dimX,dimY)) (xMax,yMax) =
  generateImage offSetter xMax yMax
        where offSetter x y
                | (x - xOff) > 0 && (x-xOff) < dimX
                &&(y - yOff) > 0 && (y-yOff) < dimY = pixelAt (convertRGB8 img) (x-xOff) (y-yOff)
                | otherwise = backingColor
              (xOff,yOff) = offset (xMax,yMax) (dimX,dimY)
              backingColor = toPixel8 $ getCommonColor img

maybeWrap :: Either a b -> Maybe b
maybeWrap (Right b) = Just b
maybeWrap _ = Nothing

dropMaybe :: Maybe (a,b) -> (Maybe a, Maybe b)
dropMaybe (Just (a,b)) = (Just a, Just b)
dropMaybe Nothing = (Nothing,Nothing)

loadImage :: FilePath -> IO (Maybe Cover)
loadImage path = do
  eithrImg <- readImageWithMetadata path
  let mImg = maybeWrap eithrImg           -- Maybe (a,b)
      imgDims = findDimensions =<< mImg   -- Maybe (a,a)
  return $ Cover <$> (fst <$> mImg) <*> imgDims <*> (pure path)
