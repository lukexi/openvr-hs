module Main where

import Graphics.VR.OpenVR

main :: IO ()
main = do
  putStrLn "Calling OpenVR"
  system <- initOpenVR
  putStrLn $ "Got system: " ++ show system
  putStrLn "Called OpenVR"
