module AuthVerification where

import Import

authVerification = (\u p -> return $ u == "fake" && p == "doNotBother")
