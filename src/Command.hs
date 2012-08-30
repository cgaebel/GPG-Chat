-- | Commands for interacting with the main executable
module Command ( genCommands
               , commandInfo
               ) where

import Language.Haskell.TH

import Util.IO
import Util.Prelewd

-- | Generate a mapping from command names to functions
genCommands :: [Name] -> ExpQ
genCommands = listE . fmap toCommand

toCommand :: Name -> ExpQ
toCommand name = tupE [ stringE $ nameBase name, global name ]

-- | Description mapping of commands by name
commandInfo :: [(Name, String)]
commandInfo = []
