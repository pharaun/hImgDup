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


--listDirectory :: RawFilePath -> IO [RawFilePath]
--listDirectory root = do
--    namesAndTypes <- getDirectoryContents root
--    let properNames = filter ((`notElem` [".", ".."]) . snd) namesAndTypes
--    paths <- forM properNames $ \(typ,name) -> unsafeInterleaveIO $ do
--        let path = root </> name
--        case () of
--            () | typ == dtDir -> allDirectoryContents path
--               | typ == dtUnknown -> do
--                    isDir <- isDirectory <$> getFileStatus path
--                    if isDir
--                        then allDirectoryContents path
--                        else return [path]
--               | otherwise -> return [path]
--    return (root : concat paths)


listDirectory :: RawFilePath -> IO [(DirType, RawFilePath)]
listDirectory root = do
    content <- liftIO (getDirectoryContents root)
    return $ filter ((`notElem` [".", ".."]) . snd) content


-- | Starting at some root directory, traverse the filesystem and enumerate
-- every file (or symlink to a file) found.
--
-- Note: This only works with POSIX platforms since it returns RawFilePath.
traverse :: MonadIO m
         => Bool -- ^ Follow directory symlinks
         -> Bool -- ^ Skip directory that we cannot access
         -> Bool -- ^ Skip any non real file
         -> RawFilePath -- ^ Root directory
         -> Producer m RawFilePath
traverse followSymLinks skipInvalidAccess skipNonRealFile root =
    liftIO (listDirectory root) >>= pull
    where
        pull [] = return ()
        pull (p:ps) = do
            return ()



-- traverse _followSymlinks root =
--     liftIO (listDirectory root) >>= pull
--   where
--     pull [] = return ()
--     pull (p:ps) = do
--         isFile' <- liftIO $ isFile p
--         if isFile'
--             then yield p >> pull ps
--             else do
--                 follow' <- liftIO $ follow p
--                 if follow'
--                     then do
--                         ps' <- liftIO $ listDirectory p
--                         pull ps
--                         pull ps'
--                     else pull ps
-- 
--     follow :: FilePath -> IO Bool
--     follow p = do
--         let path = encodeString p
--         stat <- if _followSymlinks
--             then Posix.getFileStatus path
--             else Posix.getSymbolicLinkStatus path
--         return (Posix.isDirectory stat)
-- 
-- 
-- findFileHash s p = getSymbolicLinkStatus p >>= \fs ->
--     if isRegularFile fs
--     then (do
--         hash <- singleFileHash p
--         case hash of
--             Nothing -> return s
--             Just h  -> print (h, p) >> return s
--          )
--     else return s
-- 
-- 
-- -- Rewrite this to catch and handle the exception on file not existing and no access so that
-- -- there is no race case
-- singleFileHash :: RawFilePath -> IO (Maybe B.ByteString)
-- singleFileHash f = do
--     fileExist <- fileExist f
--     if fileExist
--         then do
--             canAccess <- fileAccess f True False False
--             if canAccess
--                 then do
-- 
-- 
-- 
-- 
-- 
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
