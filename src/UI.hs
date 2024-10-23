module UI where
import System.Process (readProcess)
import Data.Foldable (find)


-- make sure you don't duplicate names
-- TODO: exit codes and gracefully failing
gumPick :: [a] -> (a -> String) -> IO (Maybe a)
gumPick items f = do
    str <- readProcess "gum" ("filter" : "--height=10" : fmap f items) ""
    -- take out \n
    let res = init str
    pure $ find ((==res). f) items