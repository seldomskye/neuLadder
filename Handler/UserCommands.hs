module Handler.UserCommands where

import Import
import Yesod.Form.Bootstrap3

-- Mark Self Active
-- FindMatch

getUserR :: Handler Html
getUserR = do
  getPlayers <- runDB $ do
          players <- selectList [] [Asc PlayerTag]
          return $ map (\x -> (playerTag x, playerTag x)) $ map entityVal players
  (playerActive, enctype) <- generateFormPost $ renderDivs $ playerActiveForm getPlayers
  defaultLayout $ do
    setTitle "NEUMelee Admin"
    $(widgetFile "topbar")
    [whamlet|<form method=post action=@{UserR}#form enctype=#{enctype}>
    ^{playerActive}
    <button .btn .btn-default type="submit">
      Set Active
|] 

postUserR :: Handler Html
postUserR = do
  getPlayers <- runDB $ do
    players <- selectList [] [Asc PlayerTag]
    return $ map (\x -> (playerTag x, playerTag x)) $ map entityVal players
  ((result, playerActive), enctype) <- runFormPost $ renderDivs $ playerActiveForm getPlayers
  case result of
    FormSuccess playerTag -> do
      madeActive <- makeActive playerTag
      defaultLayout $ do
        setTitle "NEUMelee Admin"
        $(widgetFile "topbar")
        [whamlet|<form method=post action=@{UserR}#form enctype=#{enctype}>
                            ^{playerActive}
                            <button .btn .btn-default type="submit">
                                                           Set Active
                                                          |]
    _ -> defaultLayout $(widgetFile "topbar")
playerActiveForm players = areq (selectFieldList players) (bfs ("Winner" :: Text)) Nothing

makeActive tag = runDB $ do
  now <- liftIO getCurrentTime
  updateWhere [PlayerTag ==. tag] [PlayerLastActive =. Just now, PlayerCurrentlyActive =. Just True]
  return True

findValidMatches player = runDB $ do
  let canPlay =  [PlayerCurrentlyActive ==. (Just True), PlayerRanking >. playerRanking player] 
  playables <- selectList canPlay [Asc PlayerRanking]
  return playables
