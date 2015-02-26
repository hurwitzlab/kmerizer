module Main where

import Bio.Core.Sequence
import Bio.Sequence.Fasta
import Control.Monad (when)
import Data.Foldable (forM_)
import Data.List (tails)
import Data.Monoid
import Data.Stringable (toString)
import Options.Applicative
import System.FilePath.Posix (joinPath, takeBaseName)
import System.Directory
import System.IO
import Text.Printf (printf)

-- # --------------------------------------------------
data Options = Options {
    inputFile :: String,
    outDir    :: String,
    kmerSize  :: Int
} deriving (Show)

-- # --------------------------------------------------
kmerize :: BioSeq a => Int -> a -> ([Char], Int, [[Char]])
kmerize n seq = (seqID, numKmers, kmers)
 where seqID = toString . seqid $ seq
       (numKmers, kmers) = findKmers n . toString . seqdata $ seq

-- # --------------------------------------------------
findKmers :: Int -> [Char] -> (Int, [[Char]])
findKmers k cs = (n - k + 1, findKmers' n k cs)
 where n = length cs
       findKmers' l k xs
         | l >= k = take k xs : findKmers' (l - 1) k (tail xs)
         | otherwise = []

-- # --------------------------------------------------
runWithOptions :: Options -> IO ()
runWithOptions opts = do
    let inFile       = inputFile opts
    let outputDir    = outDir opts
    let baseName     = takeBaseName inFile
    let outFileKmers = joinPath [outputDir, baseName ++ ".kmers"]
    let outFileLoc   = joinPath [outputDir, baseName ++ ".locs"]

    outdirExists <- doesDirectoryExist outputDir
    when (not outdirExists) (createDirectory outputDir)

    input <- readFasta inFile
    let kmers = map (kmerize (kmerSize opts)) input

    withFile outFileLoc WriteMode (\h -> do
        forM_ kmers $ \(kmerID, numKmers, _) ->
           hPutStrLn h $ printf "%s\t%d" kmerID numKmers
        )

    withFile outFileKmers WriteMode (\h -> do
        forM_ (zip [1..] (concatMap thrd kmers)) $ \(n, kmer) ->
           hPutStrLn h $ printf "%s\t%s" (show n) kmer
        )

    putStrLn $ printf "Done, wrote files to '%s'" outputDir
 where thrd (a,b,c) = c

-- # --------------------------------------------------
main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = Options <$> strOption
                         ( long "input"
                         <> short 'i'
                         <> metavar "INPUT" )
                     <*> strOption
                            ( long "outdir"
                           <> short 'o'
                           <> value "."
                           <> metavar "OUTDIR" )
                     <*> option auto
                            ( long "kmer"
                           <> short 'k'
                           <> value 20
                           <> metavar "KMER_SIZE" )
    opts = info parser mempty
