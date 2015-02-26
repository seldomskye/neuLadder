module Handler.RecentMatches where

import Import
import Yesod.Form.Bootstrap3
import Data.Time


easternTimeZone :: TimeZone
easternTimeZone = TimeZone
  { timeZoneMinutes =  (-18000)
  , timeZoneSummerOnly = False
  , timeZoneName = "EST"} 

renderTimeEtc :: UTCTime -> String
renderTimeEtc matchD = formatTime defaultTimeLocale "%Y-%m-%d  %H:%M" estTime
  where estTime = utcToLocalTime easternTimeZone matchD 
 
getRecentMatchesR :: Handler Html
getRecentMatchesR = do
  matches <- lastNMatches 20
  defaultLayout $ do
    setTitle "Recent Matches"
    $(widgetFile "recentMatches")


lastNMatches :: Int -> Handler [Match]
lastNMatches num = runDB $ do
  matchesByDesc <- selectList [] [Desc MatchDate]
  return $ map entityVal matchesByDesc
