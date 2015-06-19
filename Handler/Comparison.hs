module Handler.Comparison where

import Import
import Yesod.Form.Bootstrap3

getComparisonR :: Handler Html
getComparisonR = do
  playerList <- getPlayers
  (compWidget, _) <- generateFormPost $ comparisonForm' playerList
  defaultLayout $ do
    setTitle "NEUMelee Player Comparison"
    $(widgetFile "comparePlayer")

postComparisonR :: Handler Html
postComparisonR = do
  playerList <- getPlayers
  ((resultComp, compWidget), _) <- runFormPost $ comparisonForm' playerList
  defaultLayout $ do
    setTitle "NEUMelee Player Comparison"
    $(widgetFile "comparePlayer")
    processResult resultComp

comparisonForm :: [(Text, Text)] -> AForm Handler (Text, Text)
comparisonForm players = (,)
  <$> areq (selectFieldList players) (bfs ("Player 1" :: Text)) Nothing
  <*> areq (selectFieldList players) (bfs ("Player 2" :: Text)) Nothing

comparisonForm' x = renderBootstrap3 BootstrapBasicForm $ comparisonForm x

getMatches :: Text -> Text -> Handler (([Entity Match], [Entity Match]))
getMatches p1 p2 = runDB $ do
  played <- selectList( [MatchWinner ==. p1, MatchLoser ==. p2]) []
  played2 <- selectList ([MatchWinner ==. p2, MatchLoser ==. p1]) []
  return (played, played2)

processResult :: FormResult (Text, Text) -> Widget
processResult (FormSuccess  (p1, p2)) = do
  (p1w, p2w) <- handlerToWidget $ getMatches p1 p2
  let p1ws = length p1w 
      p2ws = length p2w
      allMatches = p1w ++ p2w
      win_rate :: Double
      win_rate = (fromIntegral p1ws) / (fromIntegral (length allMatches)) * 100
      matchesSortedByDate = sortBy (\ x y -> compare (matchDate x) (matchDate y)) $ fmap remEntity allMatches 
  $(widgetFile "matchResult")

remEntity :: Entity b  -> b
remEntity (Entity _ x) = x
