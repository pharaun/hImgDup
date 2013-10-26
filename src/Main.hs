-- Posix support
import System.Posix.Files.ByteString
import System.Posix.Types
import System.Posix.IO.ByteString
import Data.Int

-- Better FilePath
import System.Posix.FilePath
import System.Posix.Directory.Traversals

import Data.ByteString.UTF8 (fromString)

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

import qualified Data.ByteString.Base16 as BS16

-- PosixFilesystem
import Data.Conduit.PosixFilesystem

-- Handle io
import qualified System.IO as FP

-- Benchmarks
import Criterion.Main
import qualified Data.Conduit.Filesystem as DCF
import qualified Filesystem.Path.CurrentOS as FPC
--    nfIO $ traverseDirectory (\s p -> return (p:s)) [] path
--    nfIO $ traverse True False path $$ CL.consume
--    nfIO $ DCF.traverse True (FPC.decode path) $$ CL.consume
--    defaultMain
--        [ bgroup "file traverse"
--            [ bench "traverseDirectory" $ nfIO $ traverseDirectory (\s p -> return (p:s)) [] path
--            , bench "ConduitTraverseBS" $ nfIO $ traverse True False path $$ CL.consume
--            , bench "ConduittraverseFP" $ nfIO $ DCF.traverse True (FPC.decode path) $$ CL.consume
--            ]
--        ]




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
--    let path = fromString "/storage/other/pictures_photos/pictures/anime_2d_peoples/"
    let path = fromString "/storage/other/pictures_photos/pictures/"

    -- warmup
    a <- (M.filter (\c -> DL.length c > 1)) `fmap` (directorySize path)
    b <- (M.filter (\c -> DL.length c > 1)) `fmap` (filesHash a)
--    c <- directoryHash path

    print b





-- Conduit traverse
directoryHash :: RawFilePath -> IO [(RawFilePath, Maybe B.ByteString)]
directoryHash p = do
    fs <- getFileStatus p
    case () of
        ()  | isDirectory fs    -> traverse True False p $$ CL.concatMapM hashMap =$ CL.consume
            | isRegularFile fs  -> hashMap p
            | otherwise         -> return []

hashMap :: RawFilePath -> IO [(RawFilePath, Maybe B.ByteString)]
hashMap p = singleFileHash p >>= \hash -> return [(p, hash)]






-- TODO: Do the hashing in a more efficient manner
filesHash :: M.Map Int64 [RawFilePath] -> IO (M.Map B.ByteString [RawFilePath])
filesHash s = CL.sourceList (M.elems s) $$ CL.foldM (F.foldlM foldFileHash) M.empty

foldFileHash :: MonadIO m => M.Map B.ByteString [RawFilePath] -> RawFilePath -> m (M.Map B.ByteString [RawFilePath])
foldFileHash s path = do
    hash <- liftIO $ singleFileHash path
    case hash of
        Just h  -> return $ M.insert h (path:(M.findWithDefault [] h s)) s
        Nothing -> return s

-- TODO: rewrite to catch exception for existing/access and just operate on that
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
        -- This is KEY to convert to handle otherwise it leaks FP
        openRaw p = openFd p ReadOnly Nothing defaultFileFlags >>= fdToHandle >>= return

toHex :: B.ByteString -> B.ByteString
toHex = BS16.encode



directorySize :: RawFilePath -> IO (M.Map Int64 [RawFilePath])
directorySize p = do
    fs <- getFileStatus p
    case () of
        ()  | isDirectory fs    -> traverse True False p $$ CL.foldM singleFileSize M.empty
            | isRegularFile fs  -> return $ M.singleton (fileSizeAsInt fs) [p]
            | otherwise         -> return M.empty

singleFileSize :: MonadIO m => M.Map Int64 [RawFilePath] -> RawFilePath -> m (M.Map Int64 [RawFilePath])
singleFileSize s path = do
    size <- liftIO $ fileSizeAsInt `fmap` getFileStatus path
    return $ M.insert size (path:(M.findWithDefault [] size s)) s

-- COff -> Int64
fileSizeAsInt :: FileStatus -> Int64
fileSizeAsInt = fromIntegral . fileSize
