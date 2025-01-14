module Common.IR.Value where

data Value a where
    Number :: Num a => a -> Value a
    String :: String -> Value String