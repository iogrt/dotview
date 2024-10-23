{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module VirtualFilesystem (serveFile, PseudoFile (..)) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List (find)
import Data.Tree
import Foreign.C.Error
import GHC.Conc (threadDelay)
import GHC.List (List)
import System.Fuse
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

type HT = ()

data PseudoFile = PseudoFile
  { pseudoFileName :: String,
    readPseudoFile :: ByteCount -> FileOffset -> IO ByteString,
    pseudoFileSize :: IO FileOffset
  }

type FS = List PseudoFile

simpleStrPseudoFile :: String -> ByteString -> PseudoFile
simpleStrPseudoFile n content =
  PseudoFile
    { pseudoFileName = n,
      readPseudoFile = \byteCount offset -> pure $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) content,
      pseudoFileSize = pure $ fromIntegral $ B.length content
    }

example :: IO ()
example =
  serveFile
    [ simpleStrPseudoFile "hello" "Hello world from hfuse!"
    ]

-- TODO: Use the either that comes in act to log errors? idk
serveFile :: FS -> IO ()
serveFile fs = fuseMain (helloFSOps fs) defaultExceptionHandler >> forever (threadDelay 1000000)

-- Inline inst working :( https://github.com/eyeinsky/hfuse/issues/9
-- serveFile fs = fuseMainInline (\_ _i -> pure ()) (\_ -> pure ()) act (helloFSOps fs) defaultExceptionHandler
-- act (Left msg) = print msg
-- act (Right ()) = pure ()

helloFSOps :: FS -> FuseOperations HT
helloFSOps fs =
  defaultFuseOps
    { fuseGetFileStat = getFileStat fs,
      fuseOpen = open fs,
      fuseRead = pseudoRead fs,
      fuseOpenDirectory = openDirectory,
      fuseReadDirectory = readDirectory fs,
      fuseGetFileSystemStats = getFileSystemStats
    }

dirStat :: FuseContext -> FileStat
dirStat ctx =
  FileStat
    { statEntryType = Directory,
      statFileMode =
        foldr1
          unionFileModes
          [ ownerReadMode,
            ownerExecuteMode,
            groupReadMode,
            groupExecuteMode,
            otherReadMode,
            otherExecuteMode
          ],
      statLinkCount = 2,
      statFileOwner = fuseCtxUserID ctx,
      statFileGroup = fuseCtxGroupID ctx,
      statSpecialDeviceID = 0,
      statFileSize = 4096,
      statBlocks = 1,
      statAccessTime = 0,
      statModificationTime = 0,
      statStatusChangeTime = 0
    }

fileStat :: FuseContext -> PseudoFile -> IO FileStat
fileStat ctx pFile = do
  size <- pseudoFileSize pFile
  pure $
    FileStat
      { statEntryType = RegularFile,
        statFileMode =
          foldr1
            unionFileModes
            [ ownerReadMode,
              groupReadMode,
              otherReadMode
            ],
        statLinkCount = 1,
        statFileOwner = fuseCtxUserID ctx,
        statFileGroup = fuseCtxGroupID ctx,
        statSpecialDeviceID = 0,
        statFileSize = size,
        statBlocks = 1,
        statAccessTime = 0,
        statModificationTime = 0,
        statStatusChangeTime = 0
      }

findPseudofile :: FS -> FilePath -> Maybe PseudoFile
findPseudofile fs path = find (\(PseudoFile name _ _) -> "/" <> name == path) fs

getFileStat :: FS -> FilePath -> IO (Either Errno FileStat)
getFileStat _ "/" = do
  Right . dirStat <$> getFuseContext
getFileStat fs path = do
  -- TODO: Do subdirs
  -- TODO: Canonicalize subpath? maybe?
  ctx <- getFuseContext
  maybe
    (pure $ Left eNOENT)
    (fmap Right . fileStat ctx)
    (findPseudofile fs path)

openDirectory :: String -> IO Errno
openDirectory "/" = return eOK
openDirectory _ = return eNOENT

readDirectory :: FS -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
readDirectory fs "/" = do
  ctx <- getFuseContext
  files <- traverse (fileInfo ctx) fs
  return $
    Right
      ( [ (".", dirStat ctx),
          ("..", dirStat ctx)
        ]
          <> files
      )
  where
    fileInfo ctx p@(PseudoFile name _ _) = (name,) <$> fileStat ctx p
readDirectory _ _ = return (Left eNOENT)

open :: FS -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
open fs path mode _flags =
  case (mode, findPseudofile fs path) of
    (_, Nothing) -> return (Left eNOENT)
    (ReadOnly, Just _) -> return (Right ())
    (_, Just _) -> return (Left eACCES)

pseudoRead :: FS -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
pseudoRead fs path _ byteCount offset =
  case findPseudofile fs path of
    Just f -> Right <$> readPseudoFile f byteCount offset
    Nothing -> return (Left eNOENT)

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats str =
  return $
    Right $
      FileSystemStats
        { fsStatBlockSize = 512,
          fsStatBlockCount = 1,
          fsStatBlocksFree = 1,
          fsStatBlocksAvailable = 1,
          fsStatFileCount = 5,
          fsStatFilesFree = 10,
          fsStatMaxNameLength = 255
        }