module AuthVerification where

authVerification = (\u p -> return $ u == "fake" && p == "doNotBother")
