module Handler.Ladder where

import Import hiding (getPlayers)
import Data.Time


getLadderR :: Handler Html
getLadderR = do
  players <- getPlayers
  defaultLayout $ do
    setTitle "NEUMelee Ladder"
    $(widgetFile "ladder")

    
-- TODO: make ladder hamlet file not code duplicate so bad?
getHomeR = getLadderR 

t = -3 * 60 * 60 

getPlayers :: Handler ([Player])
getPlayers = runDB $ do
  time <- liftIO getCurrentTime
  let oldActive = [PlayerCurrentlyActive <. (Just True), PlayerLastActive <. (Just $ addUTCTime t time)]
  updateWhere oldActive [PlayerCurrentlyActive =. (Just False)]
  selectList [] [Asc PlayerRanking] >>= mapM (\(Entity _ p) -> do
                                                 return p)
getRulesR = defaultLayout $ do
  setTitle "NEUMelee Ladder Rules"
  $(widgetFile "rules")




