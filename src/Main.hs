module Main where

import Examples (demoNewAPI)

-- | Main entry point demonstrating the refactored Haskell music generation system
main :: IO ()
main = do
  putStrLn "EuterpeaFun - Refactored Haskell Music Generation System"
  putStrLn "==========================================================="
  putStrLn ""
  putStrLn "This demonstrates the new clean architecture with:"
  putStrLn "- Fixed choose/chooseMany functions"
  putStrLn "- Unified MusicGenerator module"
  putStrLn "- Consolidated Scales module"
  putStrLn "- Clean API: createGenFromScaleAndMode and createPattern"
  putStrLn ""
  putStrLn "Run 'demoNewAPI' from Examples module to hear the patterns!"
  putStrLn ""
  demoNewAPI
