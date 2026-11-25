# facial-geometry-hs

Zero-dependency Haskell library for computing 2D facial landmark geometry metrics (canthal tilt, facial thirds balance, golden ratio delta, and bilateral mirror symmetry).

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
  print $ canthalTiltDeg (Point2D 100 200) (Point2D 140 195)
  print $ computeFacialThirds 100 200 300 400
```

## Reference & Online Calculator

Interactive visual verification and landmark definitions:
- [Looksmax Scale](https://looksmaxscale.com/)
- [Canthal Tilt Measurement Guide](https://looksmaxscale.com/canthal-tilt)
- [Facial Proportion Reference](https://looksmaxscale.com/attractiveness-test)

## License

MIT
