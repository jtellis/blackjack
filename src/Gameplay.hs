module Gameplay (
    getAction
) where

data PlayerAction = Hit | Stay deriving (Show)

encodeAction :: String -> Maybe PlayerAction
encodeAction s
    | s == "H" || s == "h" = Just Hit
    | s == "S" || s == "s" = Just Stay
    | otherwise = Nothing

getAction :: IO PlayerAction
getAction = do
    s <- getLine
    maybe getAction return (encodeAction s)
