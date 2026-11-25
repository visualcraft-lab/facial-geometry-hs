# facial-geometry-hs

Haskell library with no dependencies beyond `base` for computing 2D facial landmark geometry metrics:

- **Canthal tilt angle**: `atan2 (medialY - lateralY) (abs (lateralX - medialX))` in degrees (positive = upward lateral tilt in top-down image coordinates).
- **Vertical facial thirds**: normalized upper/middle/lower segments from strictly increasing `trichionY < glabellaY < subnasaleY < mentonY` plus an L1-based balance score in `[0, 1]`.
- **Golden ratio delta**: `faceHeight / faceWidth` and its signed deviation from `phi = (1 + sqrt 5) / 2`.
- **Bilateral symmetry**: `1 - (meanMirrorError / faceWidth)` clamped to `[0, 1]` across left/right landmark pairs.

All functions validate finite inputs and return `Either String a` on invalid geometry or non-finite values.

## Install

Add `facial-geometry` to your `.cabal` file `build-depends`:

```cabal
build-depends:
    base >= 4.14 && < 5,
    facial-geometry >= 0.1
```

## Example

```haskell
import Graphics.FacialGeometry

main :: IO ()
main = do
  -- Right 7.125016348901798
  print $ canthalTiltDeg (Point2D 100 200) (Point2D 140 195)
  -- Right (FacialThirds {upperThird = 0.3333..., ..., balanceScore = 1.0})
  print $ computeFacialThirds 100 200 300 400
  -- Left "dimensions must be finite"
  print $ goldenRatioDelta (0 / 0) 100
```

## Online Tool

Companion web calculator and landmark diagrams: [Looksmax Scale](https://looksmaxscale.com/).

## License

MIT
