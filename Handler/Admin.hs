module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3

-- Would also be nice to include Control.Lens instead of some of the other shit I do 


getAdminR :: Handler Html
getAdminR = do
  playerList <- getPlayers
  (addPlayerWidget, enctype) <- generateFormPost $ playerForm
  (addMatchWidget, _) <- generateFormPost $ matchForm playerList
  (removePlayerWidget, _) <- generateFormPost $ rPlayerForm
  (updatePlayerWidget, _) <- generateFormPost $ playerUpdateForm playerList
  defaultLayout $ do
    setTitle "NEUMelee Admin"
    $(widgetFile "addMatch")
    $(widgetFile "addPlayer")
    $(widgetFile "updatePlayer")
    $(widgetFile "removePlayer")

getPlayers :: Handler [(Text, Text)]
getPlayers = runDB $ do
  players <- selectList [] [Asc PlayerTag]
  return $ map (\x -> (playerTag x, playerTag x)) $ map entityVal players

-- this can definetly be expressed significantly cleaner

playerForm = identifyForm "addPlayer"$ renderBootstrap3 BootstrapBasicForm addForm
matchForm x =  identifyForm "addMatch" $ renderBootstrap3 BootstrapBasicForm $ matchAddForm x
rPlayerForm =  identifyForm "removePlayer" $ renderBootstrap3 BootstrapBasicForm removePlayerForm
playerUpdateForm x = identifyForm "updatePlayer" $ renderBootstrap3 BootstrapBasicForm $ updatePlayerForm x

-- Is there really no better way to express this?
-- Maybe take in a list or something?
-- filter non form success and run that display function?
postAdminR :: Handler Html
postAdminR = do
  playerList <- getPlayers
  ((resultPlayer, addPlayerWidget), enctype) <- runFormPost $ playerForm
  ((resultMatch, addMatchWidget), _) <- runFormPost $ matchForm playerList
  ((resultRPlayer, removePlayerWidget), _) <- runFormPost $ rPlayerForm
  ((resultUPlayer, updatePlayerWidget), _) <- runFormPost $ playerUpdateForm playerList
  let adminWid = do
        setTitle "NEUMelee Admin"
        $(widgetFile "addMatch")
        $(widgetFile "addPlayer")
        $(widgetFile "updatePlayer")
        $(widgetFile "removePlayer")
  case (resultPlayer, resultMatch, resultUPlayer, resultRPlayer) of
    (FormSuccess player, _, _, _) -> do
      playerId <- processPlayer player 
      case playerId of
        Right _ -> defaultLayout $ do
          [whamlet|<p class="text-success">#{show player}|]
          adminWid
        Left _ -> defaultLayout $ do
          [whamlet|<p class="text-danger">Player already present|]
          adminWid
    (_, FormSuccess match, _, _) -> do
      rez <- matchProcess match
      case rez of
        Right (Match win lose _) -> defaultLayout $ do
          [whamlet|<div class="text-success">
             <p>Congrats to #{show win}!
             <p>#{show lose} a loser is you.|]
          adminWid
        Left msg -> defaultLayout $ do
          [whamlet|<p class="text-danger">#{show msg}|] 
          adminWid
    (_, _, FormSuccess playerUpdate, _) -> do
      rez <- updatePlayerProcess playerUpdate
      defaultLayout $ do
        [whamlet|<p class="text-success">#{show rez}|]
        adminWid
    (_, _, _, FormSuccess player) -> do
      rez <- playerRProcess player
      case rez of
        Right x -> defaultLayout $ do
          [whamlet|<p class="text-success">Removed #{x}|]
          adminWid
        Left x -> defaultLayout $ do
          [whamlet|<p class="text-danger">Player #{x} does not exist|]
          adminWid
    _ -> defaultLayout $ do [whamlet||]

-- Should be in a different file
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

-- rename this to something better, process means literally nothing
processPlayer :: PlayerF -> Handler (Either Player PlayerId)
processPlayer player = runDB $ do
  x <- count [PlayerRanking >. 0]
  time <- liftIO getCurrentTime
  maybePlayerId <- insertBy $ playerFtoPlayer player (x + 1) time
  case maybePlayerId of
    Left (Entity _ play) -> return $ Left play
    Right playerId -> return $ Right playerId


data PlayerF = PlayerF
              { tag :: Text
              , nameF :: Text
              , characters :: [Character]
              } deriving (Show)
convToInt64 :: Int -> Int64
convToInt64 = fromIntegral


-- | Function which converts a player form result to the internal representation of player which is used
playerFtoPlayer :: PlayerF -> Int -> UTCTime -> Player
playerFtoPlayer (PlayerF x y z) i time=
  Player x (convToInt64 i)  (map (pack . show)  z)  y (Just time) (Just True)

addForm ::AForm Handler PlayerF
addForm = PlayerF
          <$> areq textField (bfs ("Player Tag" :: Text)) Nothing
          <*> areq textField (bfs ("Player Name" :: Text)) Nothing
          <*> areq (checkboxesFieldList charactersEnum) (bfs ("Characters Played" :: Text)) Nothing 

testSettings :: FieldSettings a
testSettings = FieldSettings
  { fsLabel = "Characters Played"
  , fsTooltip = Nothing
  , fsId = Just "Test"
  , fsName = Nothing
  , fsAttrs =  [("class", "cols-xs-4")]
  }
 
 
--------------------------------------------------------------------------------
matchAddForm :: [(Text, Text)] -> AForm Handler Match
matchAddForm players = Match
               <$> areq (selectFieldList players) (bfs ("Winner" :: Text)) Nothing
               <*> areq (selectFieldList players) (bfs ("Loser" :: Text)) Nothing
               <*> lift (liftIO getCurrentTime)

-- Need to change how I access player fields away from using pattern matching
-- To tightly couples the code when I'll I"m doing is accessing some data"

matchProcess :: Match -> Handler (Either String Match)
matchProcess match@(Match win lose date)= runDB $ do
  winner <- getBy (UniqueTag win)
  loser <- getBy (UniqueTag lose)
  now <- liftIO getCurrentTime
  case (winner, loser) of
    (Just winn, Just loss) -> case (validateMatch winn loss ) of
      True -> 
        do
          _ <- insert match
          update k1 makeActive
          update k2 makeActive
          case r1 > r2 of
            True -> do
              updateWhere [PlayerRanking >=. lowest, PlayerRanking <. highest] [PlayerRanking +=. 1]
              update k1 $ [PlayerRanking =. lowest]
              return $ Right (Match win lose date)
            _ -> do
              return $ Right (Match win lose date)
        where
          (Entity k1 (Player _ r1 _ _ _ _)) = winn
          (Entity k2 (Player _ r2 _ _ _ _)) = loss
          [lowest, highest] = sort [r1, r2]
          makeActive = [PlayerLastActive =. Just now, PlayerCurrentlyActive =. Just True]
      False -> return $ Left "Invalid match"
    _ -> return $ Left "Failure"


validateMatch :: Entity Player -> Entity Player -> Bool 
validateMatch (Entity _ p1) (Entity _ p2) = (playerRanking p1) /= (playerRanking p2)
--------------------------------------------------------------------------------
removePlayerForm :: AForm Handler Text
removePlayerForm = areq textField (bfs("Player Tag" :: Text)) Nothing

playerRProcess :: Text -> Handler (Either Text Text)
playerRProcess tag = runDB $ do  
  playerWithTag <- selectList [PlayerTag ==. tag] []
  case playerWithTag of
    ((Entity idi (Player ptag pRanking _ _ _ _) ):[]) -> do
      delete idi
      updateWhere [PlayerRanking >. pRanking] [PlayerRanking -=. 1]
      return $ Right ptag
    [] -> return $ Left tag
    _ -> return $ Left $ pack "Could not find player with the specified tag."
--------------------------------------------------------------------------------
updatePlayerForm ::[(Text, Text)] -> AForm Handler (Text, Maybe [Character] , Maybe Text)
updatePlayerForm players = (,,)
                           <$> areq (selectFieldList players) (bfs ("Player Tag" :: Text)) Nothing
                           <*> aopt (checkboxesFieldList charactersEnum) (bfs ("Characters Played" :: Text)) Nothing
                           <*> aopt textField (bfs ("New Player Tag" :: Text)) Nothing



updatePlayerProcess (tag, Just characters, Nothing) =  runDB $ do
  updateWhere [PlayerTag ==. tag] [PlayerCharacters =. (map (pack . show)  characters)]
  return (tag ++ " changed used characters.")
  

updatePlayerProcess (tag, Nothing, Just newName) = runDB $ do
  updateWhere [PlayerTag ==. tag] [PlayerTag =. newName]
  updateHistory tag newName
  return (tag ++ " changed name.")

updatePlayerProcess (tag, Just characters, Just newName) = runDB $ do
  updateWhere [PlayerTag ==. tag] [PlayerTag =. newName, PlayerCharacters =. (map (pack . show)  characters)]
  updateHistory tag newName
  return (tag ++ " changed name and characters.")
 

updateHistory oldtag newtag =  do
  updateWhere [MatchWinner ==. oldtag] [MatchWinner =. newtag]
  updateWhere [MatchLoser ==. oldtag] [MatchLoser =. newtag]
