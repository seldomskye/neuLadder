-- Mark Self Active
-- FindMatch

getUserR :: Handler Html
getUserR = do
  

makeActive tag = runDB $ do
  now <- liftIO getCurrentTime
  updateWhere [UniqueTag .= tag] [PlayerLastActive .= now, PlayerCurrentlyActive .= Just True]

findValidMatches player = runDB $ do
  let canPlay =  [PlayerCurrentlyActive .== (Just True), PlayerRanking .> playerRanking player] 
  playables <- selectList canPlay [Asc PlayerRanking]
