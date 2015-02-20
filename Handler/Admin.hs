module Handler.Admin where

import Import
import Control.Exception (catch)
import Data.Maybe
import Yesod.Form.Bootstrap3
import Data.Time.Clock (getCurrentTime)

getAdminR :: Handler Html
getAdminR = do 
  (addPlayerWidget, enctype) <- generateFormPost $ playerForm
  (addMatchWidget, _) <- generateFormPost $ matchForm
  (removePlayerWidget, _) <- generateFormPost $ rPlayerForm
  defaultLayout $ do
    setTitle "NEUMelee Admin"
    $(widgetFile "topbar")
    $(widgetFile "addMatch")
    $(widgetFile "addPlayer")
    $(widgetFile "removePlayer")
 
playerForm = identifyForm "addPlayer"$ renderBootstrap3 BootstrapBasicForm addForm
matchForm =  identifyForm "addMatch" $ renderBootstrap3 BootstrapBasicForm matchAddForm
rPlayerForm =  identifyForm "removePlayer" $ renderBootstrap3 BootstrapBasicForm removeplayerForm


postAdminR :: Handler Html
postAdminR = do
  ((resultPlayer, addPlayerWidget), enctype) <- runFormPost $ playerForm
  ((resultMatch, addMatchWidget), _) <- runFormPost $ matchForm
  ((resultRPlayer, removePlayerWidget), _) <- runFormPost $ rPlayerForm
  let adminWid = do
        setTitle "NEUMelee Admin"
        $(widgetFile "topbar")
        $(widgetFile "addMatch")
        $(widgetFile "addPlayer")
        $(widgetFile "removePlayer")
  case resultPlayer of
    FormSuccess player -> do
      id <- processPlayer player
      case id of
            Right idz -> defaultLayout $ do
              adminWid
              [whamlet|<p>#{show player}|]
            Left play -> defaultLayout $ do
              adminWid
              [whamlet|<p>Player already present|]
    _ -> case resultMatch of
      FormSuccess match -> do
        rez <- matchProcess match
        case rez of
          Right (Match win lose) -> defaultLayout $ do
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

getLastPlayer = runDB $ do
  x <- count [PlayerRanking >. 0]
  return x

processPlayer :: PlayerF -> Handler (Either Player PlayerId)
processPlayer player = runDB $ do
  x <- count [PlayerRanking >. 0]
  time <- liftIO getCurrentTime
  play <- insertBy $ playerFtoPlayer player (x + 1) time
  case play of
    Left (Entity _ play) -> return $ Left play
    Right id -> return $ Right id


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
matchAddForm :: AForm Handler Match
matchAddForm = Match
               <$> areq textField (bfs ("Winner" :: Text)) Nothing
               <*> areq textField (bfs ("Loser" :: Text)) Nothing

matchProcess :: Match -> Handler (Either String Match)
matchProcess (Match win lose)= runDB $ do
  winner <- getBy (UniqueTag win)
  loser <- getBy (UniqueTag lose)
  case (winner, loser) of
    (Just winn, Just loss) -> case (validateMatch winn loss ) of
      True ->
        do
          let
            (Entity k1 p1) = winn
            (Entity k2 p2) = loss
            r1 = playerRanking p1
            r2 = playerRanking p2
            [lowest, highest] = sort [r1, r2]
            change = r1 > r2
          case change of
            True -> do
              updateWhere [PlayerRanking >=. lowest, PlayerRanking <. highest] [PlayerRanking +=. 1]
              insert $ Match k1 k2
              update k1 [PlayerRanking =. lowest]
              return $ Right (Match win lose)
            _ -> return $ Right (Match win lose)
      False -> return $ Left "Invalid match, players too far apart"
    _ -> return $ Left "Failure"


validateMatch :: Entity Player -> Entity Player -> Bool
--validateMatch (Entity _ (Player _ r1 _)) (Entity _ (Player _ r2 _)) = (<3) . abs $ r2 - r1 
validateMatch _ _ = True
--------------------------------------------------------------------------------
removeplayerForm = areq textField (bfs("Player Tag" :: Text))Nothing

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
