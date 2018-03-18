{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import Debug.Trace
import Vision.Histogram hiding (map)
import Vision.Image hiding (map,Image)
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

-- TODO look into using JuicyPixel's Palletize
-- Given a valid file, will return a triple of 256 RGB values (R,G,B)
getCommonColor :: FilePath -> IO ColorRGB
getCommonColor image = do
    imgBuf <- readImage image
    let io1 = (toFridayRGB . convertRGB8) <$> imgBuf
    case io1 of
      (Right (rgb1 :: RGB)) -> do
          -- generalize to 16 bit color. using anything higher gets too specific
          let histo = histogram (Just $ ix3 16 16 16) rgb1 :: Histogram DIM3 Int32
              -- pulls the tuple of (color frequency,(r,g,b,)) (aka assocs) from histo
              -- swap them so we can sort on the frequency, then take the highest
              mostCommon = rgbVals $ maximum $ map swap $ assocs histo
          return $ to256 mostCommon
                    -- pull values from DIM3 and convert to 256 depth. 
                    -- take the average of the bins. Use arithmetic-foo to keep a pure Integer type
              where to256 (x,y,z) = (8*(2*x+1),8*(2*y+1),8*(2*z+1))
                    rgbVals (_, Z :. x :. y :. z) = (x,y,z)
      _ -> return (0,0,0)

-- generalize to 16 bit color. using anything higher gets too specific
-- pulls the tuple of (color frequency,(r,g,b,)) (aka assocs) from histo
-- swap them so we can sort on the frequency, then take the highest
-- pull values from DIM3 and convert to 256 depth. 
-- take the average of the bins. Use arithmetic-foo to keep a pure Integer type
getCommonColor' :: DynamicImage -> ColorRGB
getCommonColor' image = to256 mostCommon
    where rgb1 = (toFridayRGB . convertRGB8) image
          histo = histogram (Just $ ix3 16 16 16) rgb1 :: Histogram DIM3 Int32
          rgbVals (_, Z :. x :. y :. z) = (x,y,z)
          mostCommon = rgbVals $ maximum $ map swap $ assocs histo
          to256 (x,y,z) = (8*(2*x+1),8*(2*y+1),8*(2*z+1))

toPixel8 :: ColorRGB -> PixelRGB8
toPixel8 (x,y,z) = PixelRGB8 (fromIntegral x) (fromIntegral y) (fromIntegral z)

findDimensions :: FilePath -> IO (Maybe Art)
findDimensions imagePath = do
    image <- readImageWithMetadata imagePath
    case image of
      (Right foo) ->
          return $ Art imagePath <$> eitherIt (pullDimns foo)
             where pullDimns (_,meta) = (toInteger <$> Meta.lookup Meta.Width meta, toInteger <$> Meta.lookup Meta.Height meta)
                   -- There HAS to be an applicative way of doing this
                   eitherIt (Just x, Just y) = Just (x,y)
                   eitherIt _ = Nothing
      _ -> return Nothing

-- Generates an x by y image of a given color
matte :: ColorRGB -> Dimensions -> Image PixelRGB8
matte (r,g,b) (x,y) = generateImage pixelRenderer (fromIntegral x) (fromIntegral y)
    where pixelRenderer z a = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- Given a larger dimensions, find the offset needed to center
offset :: (Int,Int) -> (Int,Int) -> (Int,Int)
offset (x,y) (x',y') = ((x-x') `div` 2,(y-y') `div` 2)

matteImage path = do
  image <- readImageWithMetadata path
  case image of
    (Right foo) -> do
      putStrLn $ show xoff
      putStrLn $ show yoff
      writePng "/tmp/imgtest.png" $ generateImage pixelRenderer 1920 1080
        where pixelRenderer x y
                | x > xoff && x < (1920 - xoff) && y > yoff && y < (1080 - yoff) = trace ("x: " ++ (show x) ++ " y: " ++ (show y) ++ " xoff: " ++ show (x-xoff) ++ " yoff: " ++ show (y-yoff) ++ " raw xoff: " ++ show xoff++ " raw yoff: " ++ show yoff) $ pixelAt rgb8img (x-xoff) (y-yoff)
                | otherwise = trace ("x: " ++ (show x) ++ " y: " ++ (show y) ++ " xoff: " ++ show (x-xoff) ++ " yoff: " ++ show (y-yoff) ++ " raw xoff: " ++ show xoff++ " raw yoff: " ++ show yoff) $ PixelRGB8 208 80 80
              (xoff,yoff) = offset (1920,1080) (399,399)
              (img,meta) = foo
              rgb8img = convertRGB8 img
    _ -> return ()
