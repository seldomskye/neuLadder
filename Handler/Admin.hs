module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3

getAdminR :: Handler Html
getAdminR = do
  playerList <- getPlayers
  (addPlayerWidget, enctype) <- generateFormPost $ playerForm
  (addMatchWidget, _) <- generateFormPost $ matchForm playerList
  (removePlayerWidget, _) <- generateFormPost $ rPlayerForm
  defaultLayout $ do
    setTitle "NEUMelee Admin"
    $(widgetFile "topbar")
    $(widgetFile "addMatch")
    $(widgetFile "addPlayer")
    $(widgetFile "removePlayer")

getPlayers = runDB $ do
          players <- selectList [] [Asc PlayerTag]
          return $ map (\x -> (playerTag x, playerTag x)) $ map entityVal players

    
playerForm = identifyForm "addPlayer"$ renderBootstrap3 BootstrapBasicForm addForm
matchForm x =  identifyForm "addMatch" $ renderBootstrap3 BootstrapBasicForm $ matchAddForm x
rPlayerForm =  identifyForm "removePlayer" $ renderBootstrap3 BootstrapBasicForm removePlayerForm


postAdminR :: Handler Html
postAdminR = do
  playerList <- getPlayers
  ((resultPlayer, addPlayerWidget), enctype) <- runFormPost $ playerForm
  ((resultMatch, addMatchWidget), _) <- runFormPost $ matchForm playerList
  ((resultRPlayer, removePlayerWidget), _) <- runFormPost $ rPlayerForm
  let adminWid = do
        setTitle "NEUMelee Admin"
        $(widgetFile "topbar")
        $(widgetFile "addMatch")
        $(widgetFile "addPlayer")
        $(widgetFile "removePlayer")
  case resultPlayer of
    FormSuccess player -> do
      playerId <- processPlayer player 
      case playerId of
            Right _ -> defaultLayout $ do
              adminWid
              [whamlet|<p>#{show player}|]
            Left _ -> defaultLayout $ do
              adminWid
              [whamlet|<p>Player already present|]
    _ -> case resultMatch of
      FormSuccess match -> do
        rez <- matchProcess match
        case rez of
          Right (Match win lose _) -> defaultLayout $ do
            adminWid
            [whamlet|<p>Congrats to #{show win}!
             <p>G-fucking-g #{show lose}|]
          Left msg -> defaultLayout $ do
            adminWid
            [whamlet|<p>#{show msg}|] 
      _ -> case resultRPlayer of
        FormSuccess player -> do
          rez <- playerRProcess player
          case rez of
           Right x -> defaultLayout $ do
             adminWid
             [whamlet|<p>Removed #{x}|]
           Left x -> defaultLayout $ do
             adminWid
             [whamlet|Player #{x} does not exist|]
        _ -> defaultLayout $ do [whamlet||]
    
      
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
                                    
               
matchProcess :: Match -> Handler (Either String Match)
matchProcess match@(Match win lose date)= runDB $ do
  winner <- getBy (UniqueTag win)
  loser <- getBy (UniqueTag lose)
  case (winner, loser) of
    (Just winn, Just loss) -> case (validateMatch winn loss ) of
      True ->
        do
          _ <- insert $ match
          case r1 > r2 of
            True -> do
              updateWhere [PlayerRanking >=. lowest, PlayerRanking <. highest] [PlayerRanking +=. 1]
              update k1 [PlayerRanking =. lowest]
              return $ Right (Match win lose date)
            _ -> return $ Right (Match win lose date)
            where
              (Entity k1 p1) = winn
              (Entity _ p2) = loss
              r1 = playerRanking p1
              r2 = playerRanking p2
              [lowest, highest] = sort [r1, r2]
      False -> return $ Left "Invalid match, players too far apart"
    _ -> return $ Left "Failure"


validateMatch :: Entity Player -> Entity Player -> Bool
--validateMatch (Entity _ (Player _ r1 _)) (Entity _ (Player _ r2 _)) = (<3) . abs $ r2 - r1 
validateMatch _ _ = True
--------------------------------------------------------------------------------
removePlayerForm :: AForm Handler Text
removePlayerForm = areq textField (bfs("Player Tag" :: Text))Nothing

playerRProcess :: Text -> Handler (Either Text Text)
playerRProcess tag = runDB $ do  
  playerWithTag <- selectList [PlayerTag ==. tag] []
  case playerWithTag of
    ((Entity idi (Player ptag pRanking _ _ _ _) ):[]) -> do
      delete idi
      updateWhere [PlayerRanking >. pRanking] [PlayerRanking -=. 1]
      return $ Right ptag
    [] -> return $ Left tag
    _ -> return $ Left $ pack "the fuck"
