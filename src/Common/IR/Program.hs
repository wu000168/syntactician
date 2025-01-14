module Common.IR.Program (Program) where

class Program a where
    run :: a -> IO ()