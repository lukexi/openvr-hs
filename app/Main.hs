module Main where

import Graphics.VR.OpenVR

main :: IO ()
main = do
  putStrLn "Calling OpenVR"
  initOpenVR
  putStrLn "Called OpenVR"
