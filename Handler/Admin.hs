module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3

getAdminR :: Handler Html
getAdminR = do
  playerList <- getPlayers
  (addPlayerWidget, enctype) <- generateFormPost $ playerForm
  (addMatchWidget, _) <- generateFormPost $ matchForm playerList
  (removePlayerWidget, _) <- generateFormPost $ rPlayerForm
  (updatePlayerWidget, _) <- generateFormPost $ uPlayerForm playerList
  defaultLayout $ do
    setTitle "NEUMelee Admin"
    $(widgetFile "addMatch")
    $(widgetFile "updatePlayer")
    $(widgetFile "addPlayer")
    $(widgetFile "removePlayer")

    
playerForm = identifyForm "addPlayer"$ renderBootstrap3 BootstrapBasicForm addForm
matchForm x =  identifyForm "addMatch" $ renderBootstrap3 BootstrapBasicForm $ matchAddForm x
rPlayerForm =  identifyForm "removePlayer" $ renderBootstrap3 BootstrapBasicForm removePlayerForm
uPlayerForm x = identifyForm "updatePlayer" $ renderBootstrap3 BootstrapBasicForm $ updatePlayerForm x

postAdminR :: Handler Html
postAdminR = do
  playerList <- getPlayers
  ((resultPlayer, addPlayerWidget), enctype) <- runFormPost $ playerForm
  ((resultMatch, addMatchWidget), _) <- runFormPost $ matchForm playerList
  ((resultRPlayer, removePlayerWidget), _) <- runFormPost $ rPlayerForm
  ((resultUPlayer, updatePlayerWidget), _) <- runFormPost $ uPlayerForm playerList
  let playerRW = playerRProcess resultRPlayer
      playerW = playerProcess resultPlayer
      matchW = matchProcess resultMatch
      playerU = playerUpdateProcess resultUPlayer
  defaultLayout $ do
    setTitle "NEUMelee Admin"
    playerRW
    playerU
    playerW
    matchW
    $(widgetFile "addMatch")
    $(widgetFile "updatePlayer")
    $(widgetFile "addPlayer")
    $(widgetFile "removePlayer")
    
      
data Character =
  Fox |
  Falco |
  Sheik |
  Marth |
  Jigglypuff |
  Peach |
  CaptainFalcon |
  IceClimbers |
  DrMario |
  Pikachu |
  Samus |
  Luigi |
  Yoshi |
  Mario |
  Ganondorf |
  Link |
  YoungLink |
  DonkeyKong |
  Ness |
  Bowser |
  Roy |
  Mewtwo |
  Pichu |
  Zelda |
  Kirby |
  MrGameandWatch  
  deriving (Show, Eq, Enum, Bounded)
           
charactersEnum :: [(Text, Character)]
charactersEnum = map (pack . show &&& id) [minBound..maxBound]


playerProcess :: FormResult PlayerF -> Widget
playerProcess (FormSuccess p) = playerSuccess p
playerProcess _ =  [whamlet||]

addPlayer :: PlayerF -> Handler (Either (Entity Player) String)
addPlayer player = runDB $ do
  x <- count [PlayerRanking >. 0]
  time <- liftIO getCurrentTime
  rez <- insertBy $ playerFtoPlayer player (x + 1) time
  case rez of
    (Right _) -> return $ Right $ show player
    (Left play) -> return $ Left play

playerSuccess' :: Either (Entity Player) String -> Widget
playerSuccess' (Left _) = [whamlet|<p class="text-danger">Player already present|]
playerSuccess' (Right player) = [whamlet|<p class="text-success">#{player} was added|]

playerSuccess :: PlayerF -> Widget
playerSuccess player = do
  playerE <- handlerToWidget $ addPlayer player
  playerSuccess' playerE
  


data PlayerF = PlayerF
              { tag :: Text
              , nameF :: Text
              , characters :: [Character]
              } deriving (Show)
convToInt64 :: Int -> Int64
convToInt64 = fromIntegral



playerFtoPlayer :: PlayerF -> Int -> UTCTime -> Player
playerFtoPlayer (PlayerF x y z) i time=
  Player x (convToInt64 i)  (map (pack . show)  z)  y (Just time) (Just True)

addForm ::AForm Handler PlayerF
addForm = PlayerF
          <$> areq textField (bfs ("Player Tag" :: Text)) Nothing
          <*> areq textField (bfs ("Player Name" :: Text)) Nothing
          <*> areq (checkboxesFieldList charactersEnum) (bfs ("Characters Played" :: Text)) Nothing 

--------------------------------------------------------------------------------
-- | Takes in a list of PlayerTag,PlayerTag and returns a form with two options containing that list
-- meant to handle registering matches
matchAddForm :: [(Text, Text)] -> AForm Handler Match
matchAddForm players = Match
               <$> areq (selectFieldList players) (bfs ("Winner" :: Text)) Nothing
               <*> areq (selectFieldList players) (bfs ("Loser" :: Text)) Nothing
               <*> lift (liftIO getCurrentTime)

matchProcess :: FormResult Match -> Widget
matchProcess (FormSuccess m) = matchSuccess m
matchProcess _ = [whamlet||]

matchSuccess :: Match -> Widget
matchSuccess m = do
  matchE <- handlerToWidget $ addMatch m
  matchSuccess' matchE

matchSuccess' :: (Either Text Match) -> Widget
matchSuccess' (Right (Match win lose _)) = [whamlet|<div class="text-success">
               <p>Congrats to #{show win}!
               <p>G-fucking-g #{show lose}|]
matchSuccess' (Left msg) = [whamlet|<div class="text-danger">
               <p>#{show msg}|]
             
-- | Given a match, check for player existence and process the match results
-- or reject the match and return Left Why
addMatch :: Match -> Handler (Either Text Match)
addMatch match@(Match win lose _) = runDB $ do
  winner <- getBy (UniqueTag win)
  loser <- getBy (UniqueTag lose)
  now <- liftIO getCurrentTime
  case (winner, loser) of
    (Just winn, Just loss) -> case (validateMatch winn loss ) of
      True -> 
        do
          _ <- insert $ match -- always add the match if players exist
          case r1 > r2 of
            True -> do
              updateWhere [PlayerRanking >=. lowest, PlayerRanking <. highest] [PlayerRanking +=. 1]
              update k1 [PlayerRanking =. lowest]
              updateWhere inMatch makeActive
              return $ success
            _ -> do
              updateWhere inMatch makeActive
              return $ success
            where
              (Entity k1 p1) = winn
              (Entity _ p2) = loss
              r1 = playerRanking p1
              r2 = playerRanking p2
              [lowest, highest] = sort [r1, r2]
              makeActive = [PlayerLastActive =. Just now, PlayerCurrentlyActive =. Just True]
              inMatch = [PlayerRanking ==. lowest] ||. [PlayerRanking ==. highest]
              success = Right match 
      False -> return $ Left "Invalid Match"
    (Nothing, _) -> return $ Left "Could not find winner."
    (_, Nothing) -> return $ Left "Could not find loser."

-- | Check if two players are allowed to match up against each other
-- currently just ensures the players do not have the same rank
validateMatch :: Entity Player -> Entity Player -> Bool 
validateMatch (Entity _ (Player _ r1 _ _ _ _)) (Entity _ (Player _ r2 _ _ _ _)) = r1 /= r2
--------------------------------------------------------------------------------
-- | Form for removing players from the ladder
removePlayerForm :: AForm Handler Text
removePlayerForm = areq textField (bfs("Player Tag" :: Text))Nothing

-- | FormResult for playerR handler
playerRProcess :: FormResult Text -> Widget
playerRProcess (FormSuccess t) = playerRSuccess t
playerRProcess _ = [whamlet||]

-- | The action of taking in a playerTag and deleting it
-- Left returns the player tag, but implies the tag was not found
-- Right returns the player tag that was deleted
-- This relies on player tags being unique
removePlayer :: Text -> Handler (Either Text Text)
removePlayer tag = runDB $ do  
  playerWithTag <- selectList [PlayerTag ==. tag] []
  case playerWithTag of
    ((Entity idi (Player _ pRanking _ _ _ _) ):[]) -> do
      delete idi
      updateWhere [PlayerRanking >. pRanking] [PlayerRanking -=. 1]
      return $ Right tag
    [] -> return $ Left tag
    _ -> return $ Left "Multiple players with tag found"



-- | Handler to actually remove players
playerRSuccess :: Text -> Widget
playerRSuccess tag = do
  removedE <- handlerToWidget $ removePlayer tag
  playerRSuccess' removedE

-- | Takes in results from removePlayer and returns widgets representing their results
playerRSuccess' :: (Either Text Text) -> Widget
playerRSuccess' (Right tag) = [whamlet|<p class="text-success">Removed #{tag}|]
playerRSuccess' (Left tag) = [whamlet|<p class="text-danger">Player #{tag} does not exist|]
--------------------------------------------------------------------------------

updatePlayerForm :: [(Text, Text)] ->AForm Handler (Text, [Character])
updatePlayerForm players = (,)
                        <$> areq (selectFieldList players) (bfs ("Winner" :: Text)) Nothing
                        <*> areq (checkboxesFieldList charactersEnum) (bfs ("Characters Played" :: Text)) Nothing 

playerUpdateProcess :: FormResult (Text, [Character]) -> Widget
playerUpdateProcess (FormSuccess p) = playerUpdate p
playerUpdateProcess _ = [whamlet||]

playerUpdate :: (Text, [Character]) -> Widget
playerUpdate player = do
  playerE <- handlerToWidget $ updatePlayer player
  playerUpdate' playerE

updatePlayer :: (Text, [Character]) -> Handler ()
updatePlayer (tag, characters) = runDB $ do
  updateWhere [PlayerTag ==. tag] [PlayerCharacters =. charList]
  where charList = (map (pack . show) characters)

playerUpdate' :: () -> Widget
playerUpdate' _ = [whamlet|<p class="text-success"> Player successfully updated|]
