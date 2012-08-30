-- | Program starts here
module Main ( main ) where

import Command
import Control.Arrow
import Data.Char
import Data.Tuple
import Language.Haskell.TH
import Util.IO
import Util.Prelewd

-- | Entry point
main :: IO ()
main = do putStr "> " >> hFlush stdout
          input <- toLowerCase <$> getLine
          runCommand input
          main

toLowerCase :: String -> String
toLowerCase = fmap toLower

exit :: String
exit = "exit"

runCommand :: String -> IO ()
runCommand s = fromMaybe (putStrLn "Command not recognized") $ lookup s commands

commands :: [(String, IO ())]
commands = ("help", help)
         : ("exit", exitSuccess)
         : $(genCommands $ fst <$> commandInfo)

help :: IO ()
help = do putStrLn "Available commands:"
          putStr $ helpMsg $ (exit, "Quit the program")
                           : (first nameBase <$> commandInfo)

helpMsg :: [(String, String)] -> String
helpMsg helps = helps >>= formatHelp (4 + foldr (max.length.fst) 0 helps)

formatHelp :: Integer -> (String, String) -> String
formatHelp columnWidth (cmd, dsc) = "\t" ++ pad columnWidth cmd ++ dsc ++ "\n"

pad :: Integer -> String -> String
pad len s = s ++ replicate (fromIntegral len - length s) ' '
