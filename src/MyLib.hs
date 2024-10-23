{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module MyLib (someFunc) where

--parse :: DotGraph String
--parse = parseDotGraph "graph hello { a; b; a -- b; b -- c; c -- a;}"


--someFunc :: IO ()
--someFunc = putStrLn (show parse)

-- trying diagrams-graphviz examples. Seems like it don't parse graphviz?? Idk...
import Data.RDF
import System.Process
import qualified Data.Text as T
import qualified Data.RDF.Vocabulary.SHACL as SH
import qualified Data.RDF.Vocabulary.XSD as XSD
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Applicative
import qualified Data.Text.IO as T
import Data.Foldable (find, Foldable (fold))
import qualified Data.Map as M
import Utils


rdfPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns"


rdfNode s = unode $ "http://www.w3.org/1999/02/22-rdf-syntax-ns#"<>s
rdfsNode s = unode $ "http://www.w3.org/2000/01/rdf-schema#"<>s


-- useful for vocabs to extract strings out of
-- don't use it for values you don't know
getUnode_ :: Node -> T.Text
getUnode_  (UNode x) = x

getUnode :: Node -> Maybe T.Text
getUnode (UNode x) = Just x
getUnode _ = Nothing

getUnodes = fmap (T.unpack .(\(UNode x) -> x))



invertMap :: (Ord v) => M.Map k v -> M.Map v k
invertMap m = M.fromList [(v, k) | (k, v) <- M.toList m]


prettyPrintNode :: PrefixMappings -> Node -> T.Text
prettyPrintNode pm (UNode t) = prefix t pm
prettyPrintNode _ (LNode (PlainL t)) = t
prettyPrintNode _ (LNode (PlainLL t _)) = t
prettyPrintNode _ (LNode (TypedL t _)) = t
prettyPrintNode _ (BNode t) = t
prettyPrintNode _ (BNodeGen t) = "TODO: Node gen"

-- Use case: Display alot of info about a node!
-- 1. What's connected to it
-- 2. It's type
displayNodeInfo :: Rdf a => RDF a -> Node -> IO ()
displayNodeInfo doc n = do
    -- might want something more performant... will have to dig into internals and specify more methods I think.
    -- as in.. implement foldable for all rdfs
  -- TODO: Need a pretty printer. Figure out how to display the info concisely
  -- <pred> <obj>
  let pp = prettyPrintNode (prefixMappings doc)
  putStrLn "-------------------"
  T.putStrLn (pp n)
  putStrLn "-------------------"
  putStrLn "Used as subject"
  mapM_ T.putStrLn ((\(Triple _ p o) -> pp p <> " a " <> pp o) <$> asSubject)
  putStrLn "Used as predicate"
  -- relates <sub> to <obj> (uncommon case i think)
  mapM_ T.putStrLn ((\(Triple s _ o) -> "relates" <> pp s <> "to" <> pp o) <$> asPredicate)
  putStrLn "Used as object"
  -- Is <pred> of <subject>
  mapM_ T.putStrLn ((\(Triple _ p o) -> "is" <> pp p <> "of" <> pp o) <$> asObject)
    where
      asSubject = query doc (Just n) Nothing Nothing
      asPredicate = query doc Nothing (Just n) Nothing
      asObject = query doc Nothing Nothing (Just n) 
  

someFunc = do
  let parser = TurtleParser Nothing Nothing
  Right (rdf::RDF TList) <- parseFile parser "/home/yogurt/code/rdf/start.ttl"
  print $ triplesOf rdf

  -- for debugging
  writeFile "alltriples.txt" $ show $ triplesOf rdf



  let classes = subjectOf <$> query rdf Nothing (Just (rdfNode "type")) (Just (rdfsNode "Class"))

  -- assume none are blank nodes:

  let classTexts = getUnodes classes

  displayNodeInfo rdf (unode "http://example.org#MealLog")
  displayNodeInfo rdf (unode "http://example.org#Recipe")

  -- TODO :Text equivalent of readProcess
  -- TODO: nice gum which abstracts all this
  resClass <- readProcess "gum" (["filter", "--height=10"]<>classTexts) ""
  -- Take \n out of res
  let selClass = init resClass


  -- instances of such class
  let instances = subjectOf <$> query rdf Nothing (Just (rdfNode "type")) (Just (unode (T.pack selClass)))

  print instances
  -- TODO: Doesn't do subclass instances yet!
  --putStrLn $ "Result was: "<> concat (getUnodes instances)

  -- Could use sparql in thefuture but now it seems better to manipulate the tree itself? idk

  -- Get classes (rdf:type rdf:Class)

  pure ()
  -- 1: Parse file
  -- show classes
  -- show instances of classes

-- feed a blank node to this of a property to see

-- either use triples or make a lens?
-- in super complicated future language you can do lenses down until you commit and actually fetch the data
-- maybe not lenses but just query building...
-- yup. Just have a "more efficient version" that allows to query a specific subject instead of all or do a node select even
-- and yes, query is just select + equals


-- this is how you get the gen nods triples
-- gen nodes are "artificial"
getTriples :: Rdf a => RDF a -> Node -> Triples
getTriples doc n = query doc (Just n) Nothing Nothing


-- actually will have to store the nodes triples or the node id or something idk

-- disregard sh:target and stuff like that for now.. just properties
properties :: Rdf a => RDF a -> Node -> [Node]
properties doc shape = objectOf <$> query doc (Just shape) (Just SH.property) Nothing


getNodeInt :: Node -> Maybe Integer
getNodeInt (LNode (TypedL i litType)) | litType == getUnode_ XSD.integer = Just (read (T.unpack i))
getNodeInt _ = Nothing


--newtype ShaclShape = ShaclProperty BNode

-- being an rdf is kinda dumb since you already know what the subject is... anyways
-- should have better query methods and implement them if you bother enough
newtype ShaclProperty a = ShaclProperty (RDF a)

shaclPropertyIsRequired :: Rdf a => ShaclProperty a -> Bool
shaclPropertyIsRequired (ShaclProperty r) =
  all
    ( (Just True ==) . fmap (>= 1) . getNodeInt  . objectOf)
    (query r Nothing (Just SH.minCount) Nothing)


-- TODO: Get sh:name / label for rendering!
shaclPropertyTarget :: Rdf a => ShaclProperty a -> Maybe T.Text
shaclPropertyTarget (ShaclProperty r) =
    listToMaybe (query r Nothing (Just SH.target) Nothing) >>= getUnode . subjectOf


data ShaclPropertyType =
    ShaclPropertyType_Class T.Text -- class name, then with it get instances list
    | ShaclPropertyType_DataType XSDDatatype

-- the "common" or "known" datatypes. I've not implemented all, just those I need
data XSDDatatype =
   XsdInteger
   | XsdDate


data Literal =
  LitInt Integer
  | LitString String
  | LitDate String -- Data.Date

getXsdDatatype :: T.Text -> Maybe XSDDatatype
getXsdDatatype t
  | unode t == XSD.integer = Just XsdInteger
  | unode t == XSD.date = Just XsdDate
  | otherwise = Nothing

-- parseLiteral :: LValue -> Maybe Literal
-- parseLiteral (PlainL s) = LitString s
-- parseLiteral (PlainLL s _) = LitString s -- don't care about language tags
-- parseLiteral (TypedL value uri) =
--   case getXsdDatatype uri of
--     XsdInteger -> LitInt $ read (T.unpack value)
--     XsdString -> LitString $ T.unpack value




shaclPropertyType :: Rdf a => ShaclProperty a -> Maybe ShaclPropertyType
shaclPropertyType (ShaclProperty r) = datatype <|> class'
    where
      -- 1st, query datatype
      datatype = fmap ShaclPropertyType_DataType . getXsdDatatype =<< getUnode =<< subjectOf <$> listToMaybe (query r Nothing (Just SH.datatype) Nothing)
      -- 2nd, query class
      class' = ShaclPropertyType_Class <$> (getUnode =<< subjectOf <$> listToMaybe (query r Nothing (Just SH._class) Nothing))



-- This could become a "make subgraph" typa deal
mkProperty :: Rdf a => RDF a -> Node -> ShaclProperty a
mkProperty doc n = ShaclProperty (mkRdf trips Nothing mempty)
  where
    trips = getTriples doc n

-- Now, do it all!

-- 1. Search classes
-- 2. find properties of classes
-- 3. create an instance abiding to the properties
-- 4. Extra: add more (non-abiding) properties if you want


-- Views:
-- Classes and their instances all in one view (in a kind of tree showing or something easier (just double gum filter works great))

