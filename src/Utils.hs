{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.RDF
import qualified Data.Text as T
import Network.URI
import Data.Maybe
import qualified Data.Map as M

-- | Monadic version of @partition@
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)


--- Rdf extra functions

escapedUNode :: Namespace -> T.Text -> Node
escapedUNode n t =
   complainerUnode 
    $ mkUri n
    $ T.pack
    $ escapeURIString isAllowedInURI
    $ T.unpack t

-- validates unode but errors out instead
-- if you dislike unexpected errors, do Either instead.
-- knowing what text failed is really important
complainerUnode :: T.Text -> Node
complainerUnode t = fromMaybe (error $ "unexpectedly couldnt escape node text: "<> T.unpack t) $ unodeValidate t

nsNode :: Namespace -> T.Text -> Node
nsNode n t = complainerUnode $ mkUri n t


-- probably not very performant but useful none the less
mapTriples :: Rdf a => (Triple -> Triple) -> RDF a -> RDF a
mapTriples f r = mkRdf (f <$> triplesOf r) bs pm
    where
      bs = baseUrl r
      pm = prefixMappings r

mapNodes :: (Node -> Node) -> Triple -> Triple
mapNodes f (Triple a b c) = Triple (f a) (f b) (f c)

-- so the map operation is: find the key which is the prefix. 
-- and use it
prefix :: T.Text -> PrefixMappings -> T.Text
prefix t (PrefixMappings m) = maybe t usePrefix  $ listToMaybe $ M.toList $ M.filter (`T.isPrefixOf` t) m
    where
        usePrefix (pref, uri) = pref <> ":" <> fromMaybe t (T.stripPrefix uri t)

prefixRdf :: Rdf a => RDF a -> RDF a
prefixRdf r =
    mapTriples (mapNodes nodeF) r
        where
            pm = prefixMappings r
            nodeF (UNode u) = UNode $ prefix u pm
            -- TODO: Should this be done for BNode aswell?
            nodeF other = other