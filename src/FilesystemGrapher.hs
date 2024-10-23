-- write a RDF graph from a file system!
-- for example:
-- filesystem:
-- notes/
-- -> notes1.md
-- -> notes2.md
-- -> noteFolder.md
-- -> -> innerNote.md
-- :notes
-- :note1 :fileFolder :notes
-- also allows processors to further analyze file contents and add attributes based on such
-- (for obsidian links!)
{-# LANGUAGE OverloadedStrings #-}

module FilesystemGrapher where

import Data.ByteString qualified as BS
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.RDF
import Data.String qualified
import Data.Text qualified as T
import System.Directory
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Posix (Fd)
import Utils
import VirtualFilesystem (PseudoFile (..))

distinguishDir :: FilePath -> IO (Either FilePath FilePath)
distinguishDir filepath = do
  isDir <- doesDirectoryExist filepath
  return $
    if isDir
      then Left filepath
      else Right filepath

mappings :: PrefixMappings
mappings = standard_ns_mappings <> ns_mappings [notesNS, noteRelationNS]

buildFSGraph :: (Rdf a) => FilePath -> IO (RDF a)
buildFSGraph path = do
  contents <- listDirectory path
  (dirs, files) <- partitionM doesDirectoryExist contents

  let noteFiles = mapMaybe (T.stripSuffix ".md" . T.pack) files

  let a = mconcat $ fileToRdf "notes" <$> noteFiles
  return (mkRdf a (Just (BaseUrl (uriOf notesNS))) mappings)

-- TODO: a bit absurd... lazy lists are a better mechanism than these handles which will just slow me down
buildGraphPseudoFile :: (Rdf a) => String -> IO (RDF a) -> PseudoFile
buildGraphPseudoFile name graphAction =
  PseudoFile
    { pseudoFileName = name <> ".ttl",
      readPseudoFile = \byteCount offset ->
        withSystemTempFile
          "dotview-rdf-notes"
          ( \_ h -> do
              graph <- graphAction
              serializer h graph
              hSeek h AbsoluteSeek (fromIntegral offset)
              BS.take (fromIntegral byteCount) <$> BS.hGetContents h
          ),
      pseudoFileSize =
        withSystemTempFile
          "dotview-rdf-notes-size"
          ( \_ h -> do
              graph <- graphAction
              serializer h graph
              size <- hFileSize h
              pure (fromIntegral size)
          )
    }
  where
    serializer = hWriteRdf (TurtleSerializer (Just (uriOf notesNS)) mappings)

-- TODO: recursiveness, and messing with absolute and relative paths ...
testFS :: IO ()
testFS = do
  (graph :: RDF TList) <- buildFSGraph "/home/yogurt/notes"
  writeRdf (TurtleSerializer (Just (uriOf notesNS)) mappings) (prefixRdf graph)

  -- writeRdf (TurtleSerializer (Just (uriOf notesNS)) mappings) (prefixRdf graph)
  fileH <- openFile "test_output.ttl" WriteMode
  hWriteRdf (TurtleSerializer (Just (uriOf notesNS)) mappings) fileH (prefixRdf graph)

  pure ()

notesPseudoFile :: PseudoFile
notesPseudoFile =
  buildGraphPseudoFile
    "notes"
    ( do
        (graph :: RDF TList) <- buildFSGraph "/home/yogurt/notes"
        pure $ prefixRdf graph
    )

-- only put notes inside it otherwise a note name can clash
notesNS :: Namespace
notesNS = mkPrefixedNS "notes" "http://example.org/notes#"

-- primarily useful because I don't want any name clashing whatsoever
noteRelationNS :: Namespace
noteRelationNS = mkPrefixedNS "noteR" "http://example.org/noteRelation#"

-- For some reason rdf4h serializer is not prefixing anything, so I must do the prefixing myself
-- Don't abuse this cuz it will break querying and comparisons, best to work with non prefixes in haskell

notesPrefix :: (Semigroup a, Data.String.IsString a) => a -> a
notesPrefix s = "http://example.org/notes" <> s

fileToRdf :: FilePath -> T.Text -> Triples
fileToRdf parentFolder name =
  uncurry (triple subject)
    <$>
    -- no folder cuz folder would need to be a node too and I don't have that yet
    [ -- (nsNode noteRelationNS "folder", unode $ T.pack parentFolder) -- TODO: unodeValidate and stuff
      (nsNode noteRelationNS "noteName", LNode $ plainL name)
    ]
  where
    -- TODO: Check no colliding! cuz file "abc de" and "abc_de" collide here!
    -- TODO: Check unodeValidate to see more cases you must transform
    subject = escapedUNode notesNS name

-- TODO: should write with parentFolder inn! so it's accessed like notes:abc/dce (the prefix will not have slash)
-- Nope, it tends to be messy on turtle language after. So yeah don't do that, but instead do just parentFolder for info if you want and do some kind of unique naming scheme? I think it'd be cleaner,
-- since your file view is pretty flat anyways
-- Do like behaviour of obsidian linking. Can do filename or path, as long as filenames don't collide
-- implementation: Do all full paths, and then do a mechanism to simplify and create more nodes!

-- could either define two nodes and do :sameAs, but wouldn't get properties of it so easily, or use blank nodes to the rescue!
-- like full path will be the node, and then using blanks in the other ttl like so:
-- :doc [:noteName "RDF"]
-- This won't be properly typed tho.. unless you do
-- :file1 a :noteName
-- :doc [:noteName :file1]
-- at this point, why not trigger it directly you know...
-- so yeah, the conclusion is blank nodes are good for matching but are not a good "typed" mechanism. Unless your reasoner comes in and says so yk
-- because this is an "open" approach...
-- but you kinda don't get intellisense anywhere anyhow... so...
