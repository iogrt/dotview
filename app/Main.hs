{-# LANGUAGE OverloadedStrings #-}

module Main where

import FilesystemGrapher
import MyLib qualified (someFunc)
import VirtualFilesystem

main :: IO ()
main = do
  putStrLn "Mounting notes"
  mountNotes

mountNotes :: IO ()
mountNotes = do
  serveFile [notesPseudoFile]
