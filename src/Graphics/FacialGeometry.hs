-- |
-- Module      : Graphics.FacialGeometry
-- Description : Pure-math 2D facial landmark geometry metrics
-- Copyright   : (c) 2026 VisualCraft Lab
-- License     : MIT
-- Maintainer  : wzzhang96@gmail.com
--
-- Zero-dependency 2D facial landmark geometry helpers in top-down image
-- coordinates (X right, Y down).
--
-- Reference formulas and interactive calculator: <https://looksmaxscale.com/>
module Graphics.FacialGeometry
  ( phi
  , Point2D (..)
  , LandmarkPair (..)
  , FacialThirds (..)
  , GoldenRatioResult (..)
  , canthalTiltDeg
  , computeFacialThirds
  , goldenRatioDelta
  , bilateralSymmetry
  ) where

-- | The golden ratio constant @(1 + sqrt 5) / 2@.
phi :: Double
phi = 1.618033988749895

-- | A 2D facial landmark point in top-down image coordinates.
data Point2D = Point2D
  { pointX :: !Double
  , pointY :: !Double
  } deriving (Eq, Show)

-- | A corresponding pair of left and right facial landmarks.
data LandmarkPair = LandmarkPair
  { leftPoint  :: !Point2D
  , rightPoint :: !Point2D
  } deriving (Eq, Show)

-- | Normalized vertical facial thirds and an overall balance score in @[0, 1]@.
data FacialThirds = FacialThirds
  { upperThird   :: !Double
  , middleThird  :: !Double
  , lowerThird   :: !Double
  , balanceScore :: !Double
  } deriving (Eq, Show)

-- | Golden ratio measurement comparing @faceHeight / faceWidth@ against 'phi'.
data GoldenRatioResult = GoldenRatioResult
  { heightWidthRatio :: !Double
  , phiDelta         :: !Double
  } deriving (Eq, Show)

-- | Compute the medial-to-lateral canthal tilt angle in degrees.
-- Positive degrees indicate an upward lateral tilt in top-down image space.
canthalTiltDeg :: Point2D -> Point2D -> Either String Double
canthalTiltDeg medial lateral
  | dx <= 1e-12 = Left "medial and lateral x coordinates must differ"
  | otherwise   = Right (atan2 dyUp dx * (180.0 / pi))
  where
    dx   = abs (pointX lateral - pointX medial)
    dyUp = pointY medial - pointY lateral

-- | Compute vertical facial thirds from strictly increasing top-down Y values.
computeFacialThirds
  :: Double -- ^ Trichion (hairline) Y coordinate
  -> Double -- ^ Glabella (brow midpoint) Y coordinate
  -> Double -- ^ Subnasale (nose base) Y coordinate
  -> Double -- ^ Menton (chin bottom) Y coordinate
  -> Either String FacialThirds
computeFacialThirds trichionY glabellaY subnasaleY mentonY
  | not (trichionY < glabellaY && glabellaY < subnasaleY && subnasaleY < mentonY) =
      Left "y coordinates must strictly increase from trichion to menton"
  | otherwise =
      let total  = mentonY - trichionY
          u      = (glabellaY - trichionY) / total
          m      = (subnasaleY - glabellaY) / total
          l      = (mentonY - subnasaleY) / total
          target = 1.0 / 3.0
          l1     = abs (u - target) + abs (m - target) + abs (l - target)
          score  = max 0.0 (min 1.0 (1.0 - (l1 / (4.0 / 3.0))))
       in Right (FacialThirds u m l score)

-- | Compute @faceHeight / faceWidth@ and its signed deviation from 'phi'.
goldenRatioDelta :: Double -> Double -> Either String GoldenRatioResult
goldenRatioDelta faceHeight faceWidth
  | faceHeight <= 0 || faceWidth <= 0 = Left "dimensions must be positive"
  | otherwise =
      let ratio = faceHeight / faceWidth
       in Right (GoldenRatioResult ratio (ratio - phi))

-- | Compute a normalized bilateral mirror symmetry score in @[0, 1]@.
bilateralSymmetry
  :: [LandmarkPair] -- ^ Non-empty list of left/right landmark pairs
  -> Double         -- ^ Midline X coordinate
  -> Double         -- ^ Positive face width
  -> Either String Double
bilateralSymmetry [] _ _ = Left "pairs must be non-empty"
bilateralSymmetry pairs midlineX faceWidth
  | faceWidth <= 0 = Left "faceWidth must be positive"
  | otherwise =
      let pairErr (LandmarkPair l r) =
            let mirroredLX = 2.0 * midlineX - pointX l
                dx = mirroredLX - pointX r
                dy = pointY l - pointY r
             in sqrt (dx * dx + dy * dy)
          sumErr  = sum (map pairErr pairs)
          meanErr = sumErr / fromIntegral (length pairs)
          score   = 1.0 - (meanErr / faceWidth)
       in Right (max 0.0 (min 1.0 score))
