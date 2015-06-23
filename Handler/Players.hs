module Handler.Players where

import Import

getPlayers = runDB $ do
          players <- selectList [] [Asc PlayerTag]
          return $ map (\x -> (playerTag x, playerTag x)) $ map entityVal players
