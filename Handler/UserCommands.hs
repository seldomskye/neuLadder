-- Mark Self Active
-- FindMatch


findValidMatches player = runDB $ do
  let canPlay =  [PlayerCurrentlyActive .== (Just True), PlayerRanking .> playerRanking player] 
  playables <- selectList canPlay [Asc PlayerRanking]
