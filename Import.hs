module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

topBar = $(widgetFile "topbar")
getPlayers :: Handler [(Text, Text)]
getPlayers = runDB $ do
          players <- selectList [] [Asc PlayerTag]
          return $ map (\x -> (playerTag x, playerTag x)) $ map entityVal players
