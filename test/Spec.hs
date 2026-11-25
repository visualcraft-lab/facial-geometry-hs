module Main (main) where

import Graphics.FacialGeometry
import System.Exit (exitFailure)

assertTrue :: String -> Bool -> IO ()
assertTrue label True  = putStrLn ("PASS: " ++ label)
assertTrue label False = putStrLn ("FAIL: " ++ label) >> exitFailure

near :: Double -> Double -> Bool
near a b = abs (a - b) <= 1e-9

main :: IO ()
main = do
  let nanVal = 0.0 / 0.0 :: Double
      infVal = 1.0 / 0.0 :: Double

  case canthalTiltDeg (Point2D 100 200) (Point2D 140 193) of
    Right deg -> assertTrue "canthalTiltDeg positive range" (deg > 9.0 && deg < 10.5)
    Left err  -> assertTrue ("unexpected error: " ++ err) False

  case canthalTiltDeg (Point2D 100 200) (Point2D 100 195) of
    Left _  -> assertTrue "canthalTiltDeg degenerate span" True
    Right _ -> assertTrue "expected error on zero span" False

  case canthalTiltDeg (Point2D nanVal 200) (Point2D 140 193) of
    Left _  -> assertTrue "canthalTiltDeg rejects NaN" True
    Right _ -> assertTrue "expected error on NaN" False

  case computeFacialThirds 100 200 300 400 of
    Right t ->
      assertTrue "computeFacialThirds balanced"
        (near (upperThird t) (1.0 / 3.0) && near (balanceScore t) 1.0)
    Left err -> assertTrue ("unexpected error: " ++ err) False

  case computeFacialThirds 0 1 2 infVal of
    Left _  -> assertTrue "computeFacialThirds rejects Infinity" True
    Right _ -> assertTrue "expected error on Infinity" False

  case goldenRatioDelta phi 1.0 of
    Right gr ->
      assertTrue "goldenRatioDelta exact phi"
        (near (heightWidthRatio gr) phi && near (phiDelta gr) 0.0)
    Left err -> assertTrue ("unexpected error: " ++ err) False

  case goldenRatioDelta nanVal 1.0 of
    Left _  -> assertTrue "goldenRatioDelta rejects NaN" True
    Right _ -> assertTrue "expected error on NaN" False

  let pairs =
        [ LandmarkPair (Point2D 160 200) (Point2D 240 200)
        , LandmarkPair (Point2D 170 260) (Point2D 230 260)
        ]
  case bilateralSymmetry pairs 200 160 of
    Right s  -> assertTrue "bilateralSymmetry perfect" (near s 1.0)
    Left err -> assertTrue ("unexpected error: " ++ err) False

  case bilateralSymmetry [LandmarkPair (Point2D infVal 200) (Point2D 240 200)] 200 160 of
    Left _  -> assertTrue "bilateralSymmetry rejects Infinity" True
    Right _ -> assertTrue "expected error on Infinity" False
