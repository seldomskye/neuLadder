module Handler.UserCommands where

import Import
import Handler.Players
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
    [whamlet|<form method=post action=@{UserR}#form enctype=#{enctype}>
    ^{playerActive}
    <button .btn .btn-default type="submit">
      Who can I challenge
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
      playables <- findValidMatches playerTag
      defaultLayout $ do
        setTitle "NEUMelee Admin"
        [whamlet||<div class="row col-xs-12">
  <table class="table table-striped">
    <thead>
      <tr>
        <th>Ranking
        <th>Player Tag
        <th>Name
        <th class="hidden-xs">Characters
    <tbody>
      $forall Player tag ranking characters name _ _ <-  playables
        <tr id=#{ranking}>
          <td>#{ranking}
          <td>#{tag}
          <td>#{name}
          <td class="hidden-xs">
            $forall char <- characters
              <img src=/static/meleeChars/#{toLower char}.png alt=#{char}>|]
        [whamlet|<form method=post action=@{UserR}#form enctype=#{enctype}>
                            ^{playerActive}
                            <button .btn .btn-default type="submit">
                                                           Who can I challenge
                                                          |]
    _ -> defaultLayout $(widgetFile "topbar")
    
playerActiveForm players = areq (selectFieldList players) (bfs ("Player Name" :: Text)) Nothing



makeActive tag = runDB $ do
  now <- liftIO getCurrentTime
  updateWhere [PlayerTag ==. tag] [PlayerLastActive =. Just now, PlayerCurrentlyActive =. Just True]
  return True

findValidMatches playerTag = runDB $ do
  mPlayer <- selectFirst [PlayerTag ==. playerTag] []
  case mPlayer of
    Nothing -> return []
    (Just player) -> do
      playables <- selectList activeHigherRank [Desc PlayerRanking]
      return $ take 3 $ map entityVal playables
      where activeHigherRank  = [PlayerCurrentlyActive ==. (Just True), PlayerRanking <. (playerRanking $ entityVal player)] 

