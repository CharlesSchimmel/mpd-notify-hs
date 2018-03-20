{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Images
    (
    ColorRGB(..)
  , Cover(..)
  , Dimensions(..)
  , findDimensions
  , getCommonColor
  , loadCover
  , matteCover
    ) where 


import Codec.Picture
import Codec.Picture.ColorQuant
import Control.Applicative
import Control.Exception
import Data.Int
import Data.List
import Data.Tuple (swap)
import Debug.Trace

import Utilities

type Dimensions = (Int, Int)

type ColorRGB = (Int,Int,Int)

data Cover = Cover { coverImg :: Image PixelRGB8, coverDimensions :: Dimensions, coverPath :: FilePath }

instance Eq Cover where
  (Cover _ d _) == (Cover _ d' _ ) = d == d'

instance Ord Cover where
  (Cover _ d _) `compare` (Cover _ d' _ ) = d `compare` d'

-- Convert to RGB8 and palettize it with 4 colors. Take the second of the tuple and grab the first pixel.
-- Maybe make which pixel is chosen random? 4 is chosen arbitrarily but gives decent results most of the time.
getCommonColor :: Image PixelRGB8 -> PixelRGB8
getCommonColor image = pixelAt pal 0 0
  where pal = snd $ palettize (PaletteOptions MedianMeanCut False 4) image

toPixel8 :: ColorRGB -> PixelRGB8
toPixel8 (x,y,z) = PixelRGB8 (fromIntegral x) (fromIntegral y) (fromIntegral z)

findDimensions :: Image a -> Dimensions
findDimensions (Image x y _) = (x,y)

-- Given a larger dimensions, find the offset needed to center
offset :: (Int,Int) -> (Int,Int) -> (Int,Int)
offset (x,y) (x',y') = ((x-x') `div` 2,(y-y') `div` 2)

-- Given (DynamicImage, Dimensions) and maximum dimensions, try to matte the image against a common colored background
-- if image is larger than maximum dimensions essentially crops it to maximum dimensions
-- offSetter calculates the relative position in the target image.
matteCover :: Cover -> (Int,Int) -> Image PixelRGB8
matteCover (Cover img (dimX,dimY) _) (xMax,yMax) =
  generateImage offSetter xMax yMax
    where offSetter x y
            | (x - xOff) > 0 && (x-xOff) < dimX
            &&(y - yOff) > 0 && (y-yOff) < dimY = pixelAt img (x-xOff) (y-yOff)
            | otherwise = getCommonColor img
          (xOff,yOff) = offset (xMax,yMax) (dimX,dimY)

-- Load into Cover datatype to store Dimensions and Filepath as well as DynamicImage
loadCover :: FilePath -> IO (Maybe Cover)
loadCover path = do
  eithrImg <- readImage path
  let mImg = convertRGB8 <$> maybeWrap eithrImg     -- Maybe img
      imgDims = findDimensions <$> mImg             -- Maybe (x,y)
  return $ Cover <$> mImg <*> imgDims <*> pure path
