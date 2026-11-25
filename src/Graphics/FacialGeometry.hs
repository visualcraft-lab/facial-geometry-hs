-- |
-- Module      : Graphics.FacialGeometry
-- Description : Pure-math 2D facial landmark geometry metrics
-- Copyright   : (c) 2026 VisualCraft Lab
-- License     : MIT
-- Maintainer  : wzzhang96@gmail.com
--
-- Pure Haskell helpers (no dependencies beyond @base@) for computing 2D facial
-- landmark geometry metrics.
--
-- All coordinates use top-down image space (@X@ increases to the right, @Y@
-- increases downward) in consistent linear units (for example pixels or
-- normalized image coordinates). Angles are returned in degrees, ratios are
-- dimensionless, and scores lie in @[0, 1]@.
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

-- | Internal check ensuring floating-point inputs are finite (neither NaN nor infinite).
isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)

-- | A 2D facial landmark point in top-down image coordinates.
data Point2D = Point2D
  { pointX :: !Double
  , pointY :: !Double
  } deriving (Eq, Show)

isFinitePoint :: Point2D -> Bool
isFinitePoint (Point2D x y) = isFinite x && isFinite y

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
--
-- Positive degrees indicate an upward lateral tilt in top-down image space.
-- The horizontal span threshold @1e-12@ is measured in the input coordinate units.
canthalTiltDeg :: Point2D -> Point2D -> Either String Double
canthalTiltDeg medial lateral
  | not (isFinitePoint medial && isFinitePoint lateral) =
      Left "landmark coordinates must be finite"
  | dx <= 1e-12 = Left "medial and lateral x coordinates must differ"
  | otherwise   = Right (atan2 dyUp dx * (180.0 / pi))
  where
    dx   = abs (pointX lateral - pointX medial)
    dyUp = pointY medial - pointY lateral

-- | Compute vertical facial thirds from strictly increasing finite top-down Y values.
computeFacialThirds
  :: Double -- ^ Trichion (hairline) Y coordinate
  -> Double -- ^ Glabella (brow midpoint) Y coordinate
  -> Double -- ^ Subnasale (nose base) Y coordinate
  -> Double -- ^ Menton (chin bottom) Y coordinate
  -> Either String FacialThirds
computeFacialThirds trichionY glabellaY subnasaleY mentonY
  | not (all isFinite [trichionY, glabellaY, subnasaleY, mentonY]) =
      Left "y coordinates must be finite"
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
  | not (isFinite faceHeight && isFinite faceWidth) =
      Left "dimensions must be finite"
  | faceHeight <= 0 || faceWidth <= 0 =
      Left "dimensions must be positive"
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
  | not (isFinite midlineX && isFinite faceWidth) =
      Left "midlineX and faceWidth must be finite"
  | faceWidth <= 0 =
      Left "faceWidth must be positive"
  | not (all (\(LandmarkPair l r) -> isFinitePoint l && isFinitePoint r) pairs) =
      Left "all landmark coordinates must be finite"
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
