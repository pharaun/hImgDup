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
--    , sourceFile
--    , sinkFile
    ) where

-- Posix support
import System.Posix.Files.ByteString
import System.Posix.Types
import System.Posix.IO.ByteString
import System.Posix.Directory.Foreign

-- Better FilePath
import System.Posix.FilePath
import System.Posix.Directory.Traversals (getDirectoryContents)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Conduit
import qualified Data.ByteString as B

import Control.Applicative ((<$>))
import           System.IO.Error


-- | This lists all items in a directory (files, sub directories, etc..) and returns
-- a tuple which indicates the file/directory type and the full path to that entity.
listDirectory :: Bool -- ^ Raise an exception if we cannot access the directory content
              -> RawFilePath -- ^ Root directory
              -> IO [(DirType, RawFilePath)]
listDirectory raiseException root = do
    content <- liftIO $ tryIOError $ getDirectoryContents root

    case content of
        Left e ->
            if isPermissionError e && (not raiseException)
                then return []
                else liftIO $ ioError e

        Right ps -> return $ map (\(t, p) -> (t, root </> p)) $ filter ((`notElem` [".", ".."]) . snd) ps




--                        ps' <- liftIO $ tryIOError $ listDirectory p
--                        case ps' of
--                            Left e ->
--                                if isPermissionError e && skipDirs p
--                                then pull ps
--                                else liftIO $ ioError e
--                            Right ps'' -> pull ps >> pull ps''



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

            -- Symlinks
            | dType == dtLnk = do
                -- Identify what the link is pointing to then keep going
                -- readSymbolicLink is not recursive
                -- TODO: symlink follow
                pull ps

            -- Unknown, check if link or a directory
            | dType == dtUnknown = do
                fs <- liftIO $ getFileStatus path
                case () of
                     () | isDirectory fs    -> liftIO (listDirectory raise path) >>= pull >> pull ps
                        | isRegularFile fs  -> yield path >> pull ps

                        | isSymbolicLink fs -> pull ps -- TODO: implement

                        | otherwise         -> pull ps

            -- Otherwise
            | otherwise = pull ps


-- -- | Same as 'CB.sourceFile', but uses system-filepath\'s @FilePath@ type.
-- sourceFile :: MonadResource m
--            => FilePath
--            -> Producer m S.ByteString
-- sourceFile = CB.sourceFile . encodeString
--
-- -- | Same as 'CB.sinkFile', but uses system-filepath\'s @FilePath@ type.
-- sinkFile :: MonadResource m
--          => FilePath
--          -> Consumer S.ByteString m ()
-- sinkFile = CB.sinkFile . encodeString
