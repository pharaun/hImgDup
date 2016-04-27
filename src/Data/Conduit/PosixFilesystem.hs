{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: 2011 John Millikin
--            2011 Michael Snoyman
--            2013 Anja Berens
-- License: MIT
--
-- Maintainer: pharaun666@gmail.com
-- Portability: Posix
--
-- Conduit-based API for manipulating the filesystem using Posix API.
--
-- This was mostly taken from conduit-filesystem and adapted to use the faster
-- Posix Bytestring API.
module Data.Conduit.PosixFilesystem
    ( traverse
    , sourceFile
    , sinkFile
    ) where

-- Posix support
import System.Posix.Directory.Foreign
import System.Posix.Files.ByteString
import System.Posix.IO.ByteString
import System.Posix.Types

-- Better FilePath
import System.Posix.FilePath
import System.Posix.Directory.Traversals (getDirectoryContents)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as B

import Control.Applicative ((<$>))
import Control.Arrow (second)
import System.IO.Error

import Conduit (MonadBaseControl, MonadResource, runResourceT)

import Prelude hiding (traverse)


-- | This lists all items in a directory (files, sub directories, etc..) and returns
-- a tuple which indicates the file/directory type and the full path to that entity.
listDirectory :: Bool -- ^ Raise an exception if we cannot access the directory content
              -> RawFilePath -- ^ Root directory
              -> IO [(DirType, RawFilePath)]
listDirectory raiseException root = do
    content <- tryIOError $ getDirectoryContents root
    case content of
        Left e ->
            if isPermissionError e && not raiseException
                then return []
                else ioError e

        Right ps -> return $ map (second (root </>)) $ filter ((`notElem` [".", ".."]) . snd) ps

-- | This identifies a file using getFileStatus which follows symbolic
-- links, then return the file identification in terms of DirType.
identifyFile :: Bool -- ^ Follow the symbolic link for directories
             -> Bool -- ^ Raise an exception if the getFileStatus fails
             -> RawFilePath -- ^ File/Directory to identify
             -> IO (Maybe (DirType, RawFilePath))
identifyFile followSymLinks raiseException path = do
    fs <- tryIOError $ getFileStatus path
    case fs of
        Left  e -> if not raiseException || not followSymLinks -- TODO: not sure this is right
                    then return Nothing
                    else ioError e

        Right s -> return $ case () of
            ()  | isDirectory s    -> if followSymLinks then Just (dtDir, path) else Nothing
                | isRegularFile s  -> Just (dtReg, path)
                | otherwise        -> Nothing

-- | Starting at some root directory, traverse the filesystem and enumerate
-- every file (or symlink to a file) found.
--
-- Note: This only works with POSIX platforms since it returns RawFilePath.
traverse :: MonadIO m
         => Bool -- ^ Follow directory symlinks
         -> Bool -- ^ Raise Exception if we can't access a directory, or fail to follow a symlink
         -> RawFilePath -- ^ Root directory
         -> Producer m RawFilePath
traverse followSymLinks raise root =
    liftIO (listDirectory raise root) >>= pull
    where
        pull [] = return ()
        pull ((dType, path):ps)
            -- Regular files
            | dType == dtReg = yield path >> pull ps

            -- Regular directory
            | dType == dtDir = do
                ps' <- liftIO (listDirectory raise path)
                pull ps
                pull ps'

            -- Otherwise
            | otherwise = do
                t <- liftIO (identifyFile followSymLinks raise path)
                case t of
                    Nothing -> pull ps
                    Just z -> pull [z] >> pull ps

-- | Same as 'CB.sourceFile', but uses unix\'s @RawFilePath@ type.
sourceFile :: MonadResource m
           => RawFilePath
           -> Producer m B.ByteString
sourceFile path = CB.sourceIOHandle $ openFd path ReadOnly Nothing defaultFileFlags >>= fdToHandle

-- | Same as 'CB.sinkFile', but uses unix\'s @RawFilePath@ type.
sinkFile :: MonadResource m
         => RawFilePath
         -> Consumer B.ByteString m ()
sinkFile path = CB.sinkIOHandle $ openFd path WriteOnly Nothing defaultFileFlags >>= fdToHandle
