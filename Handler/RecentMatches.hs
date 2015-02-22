module Handler.RecentMatches where

import Import
import Yesod.Form.Bootstrap3

getRecentMatchesR :: Handler Html
getRecentMatchesR = do
  matches <- lastNMatches 20
  defaultLayout $ do
    setTitle "Recent Matches"
    $(widgetFile "topbar")
    $(widgetFile "recentMatches")


lastNMatches :: Int -> Handler [Match]
lastNMatches num = runDB $ do
  matchesByDesc <- selectList [] [Desc MatchDate]
  return $ map entityVal matchesByDesc
