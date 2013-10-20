-- Posix support
import System.Posix.Files.ByteString
import System.Posix.Types
import System.Posix.IO.ByteString
import Data.Int

-- Better FilePath
import System.Posix.FilePath
import System.Posix.Directory.Traversals

import Data.ByteString.UTF8 (fromString, toString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.List as DL

-- Consider HashMap for o(1) lookups
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F

-- Conduit hashing
import Control.Exception (bracket)
import Data.Word (Word8)
import Numeric (showHex)

import Control.Monad
import qualified Data.Conduit.List as CL

-- Conduit
import Crypto.Conduit (sinkHash, hashFile)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Conduit (($$), (=$), runResourceT, MonadResource, Source, yield, bracketP)
import Data.Serialize (encode)

import Crypto.Hash.CryptoAPI


-- PosixFilesystem
import Data.Conduit.PosixFilesystem

-- Handle io
import qualified System.IO as FP

-- Benchmarks
import Criterion.Main


-- 1. Scan directories for duplicate files
--     a. winnow it down by size
--         I. scan and group files by size that way we only need to hash/file compare files in the same size group
--         II. Remove all grouping of 1 files so its only 2 or more
--
--     b. Either perform a hash of the entire file
--         I. Easy, just hash each file and compare, and can use for future comparsion
--         II. Possible waste of disk io
--         III. Partial hashing of the file. Then compare the hit/miss rate
--
--     c. Or compare it 1:1 byte wise
--         I. Can lead to rapid elimation since difference probably will appear early on
--         II. Can lead to complicated multi-way compare, which can be bad file IO
--         III. Can be complicated for comparing, IE 4 files and all 4 are same in pairs, ie A<->B, C<->D.
--         IV. Can probably do a hybrid algo that checks a few bytes in the file before checksumming to identify if its worth checksumming or not.
--         V. Scan it in certain way to.. guess the chance of a duplicate
--             > It starts by grouping files by size.
--             >
--             > Then it starts reading small chunks of the files of the same size and
--             > comparing them. It creates smaller groups depending on these comparisons.
--             >
--             > It goes on with bigger and bigger chunks (of size up to a hard-coded limit).
--             >
--             > It stops reading from files as soon as they form a single-element group or
--             > they are read completely (which only happens when they have a very high
--             > probability of having duplicates).
--             >
--             > log2
--
--     d. Finally compare the files that hashes same with 1:1 byte compare
--         I. If use weak but fast hashing its probably wise
--         II. Can also just not do this and *bet* on the fact that the odds of two different file hasing the same is pretty remote for a good hash.
--


main :: IO ()
main = do
    -- Ideally have a fancy command line parser here but for now just have a vanilla string
--    let path = fromString "/storage/other/pictures_photos/pictures"
    let path = fromString "/storage/other/pictures_photos/pictures/anime_2d_peoples/"

    -- warmup
    nfIO ((M.filter (\c -> DL.length c > 1)) `fmap` (directory path))
    nfIO (directoryHash path)
    nfIO (hashDirectory path)

    defaultMain
        [ bgroup "file traverse hash"
            [ bench "traverseDirectory" $ nfIO $ hashDirectory path
            , bench "Conduittraverse"   $ nfIO $ directoryHash path
            ]
        ]


hashDirectory :: RawFilePath -> IO [(RawFilePath, Maybe B.ByteString)]
hashDirectory p = getSymbolicLinkStatus p >>= \fs ->
    if isRegularFile fs
    then (do
            hash <- hashMap p
            return hash
         )
    else if isDirectory fs
         then traverseDirectory findFileHash [] p >>= return
         else return []

findFileHash :: [(RawFilePath, Maybe B.ByteString)] -> RawFilePath -> IO [(RawFilePath, Maybe B.ByteString)]
findFileHash s p = getSymbolicLinkStatus p >>= \fs ->
    if isRegularFile fs
    then (do
        hash <- hashMap p
        return hash
        )
    else return s


-- Conduit traverse
directoryHash :: RawFilePath -> IO [(RawFilePath, Maybe B.ByteString)]
directoryHash p = do
    fs <- getFileStatus p
    case () of
        ()  | isDirectory fs    -> traverse True False p $$ CL.concatMapM hashMap =$ CL.consume
            | isRegularFile fs  -> do
                a <- hashMap p
                return a
            | otherwise         -> return []

hashMap :: RawFilePath -> IO [(RawFilePath, Maybe B.ByteString)]
hashMap p = do
    hash <- singleFileHash p
--    print $ show (p, hash)
    return [(p, hash)]


-- Rewrite this to catch and handle the exception on file not existing and no access so that
-- there is no race case
singleFileHash :: RawFilePath -> IO (Maybe B.ByteString)
singleFileHash f = do
    fileExist <- fileExist f
    if fileExist
        then do
            canAccess <- fileAccess f True False False
            if canAccess
                then do
                    digest <- runResourceT $ bigSourceFile f $$ sinkHash
                    let hash = encode (digest :: MD5)

                    -- Should hexify this
                    return $ Just $ toHex hash

                else return $ Nothing
        else return $ Nothing



sourceHandle :: MonadIO m => FP.Handle -> Source m B.ByteString
sourceHandle h = loop
    where
        loop = do
            bs <- liftIO (B.hGetSome h (1024*1024))
            unless (B.null bs) $ yield bs >> loop





-- Custom bigBlock Conduit sourceFile with 1MiB blocks
bigSourceFile :: MonadResource m => RawFilePath -> Source m B.ByteString
bigSourceFile file = bracketP (openRaw file) FP.hClose sourceHandle
    where
        openRaw p = do
            fd <- openFd p ReadOnly Nothing defaultFileFlags
            handle <- fdToHandle fd
            return handle






-- TODO: fix this up, this is inefficient, write it in unfoldR
toHex :: B.ByteString -> B.ByteString
toHex = B.concatMap word8ToHex
    where
        word8ToHex :: Word8 -> B.ByteString
        word8ToHex w = C.pack $ pad $ showHex w []

        -- We know that the input will always be 1 or 2 characters long.
        pad :: String -> String
        pad [x] = ['0', x]
        pad s   = s





-- File size n' stuff the traverseDirectory works good here
directory :: RawFilePath -> IO (M.Map Int64 [RawFilePath])
directory p = getSymbolicLinkStatus p >>= \fs ->
    if isRegularFile fs
    then return $ M.singleton (fileSizeAsInt fs) [p]
    else if isDirectory fs
         then traverseDirectory findFileSize M.empty p
         else return M.empty

findFileSize :: M.Map Int64 [RawFilePath] -> RawFilePath -> IO (M.Map Int64 [RawFilePath])
findFileSize s p = getSymbolicLinkStatus p >>= \fs ->
    return $ if isRegularFile fs
             then M.insert (fileSizeAsInt fs) (p:(M.findWithDefault [] (fileSizeAsInt fs) s)) s
             else s

-- COff -> Int64
fileSizeAsInt :: FileStatus -> Int64
fileSizeAsInt = fromIntegral . fileSize
