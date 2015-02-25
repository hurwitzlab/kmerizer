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
kmerize :: BioSeq a => Int -> a -> ([Char],[[Char]])
kmerize n seq =
    ( toString $ seqid seq,
      findKmers n $ toString $ seqdata seq
    )

-- # --------------------------------------------------
findKmers :: Int -> [Char] -> [[Char]]
findKmers n = takeWhile (\s -> length s == n) . map (take n) . tails

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
        forM_ kmers $ \(i, k) ->
           hPutStrLn h $ printf "%s\t%d" i (length k)
        )

    withFile outFileKmers WriteMode (\h -> do
        forM_ (zip [1..] (concatMap snd kmers)) $ \(n, kmer) ->
           hPutStrLn h $ printf "%s\t%s" (show n) kmer
        )

    putStrLn $ printf "Done, wrote files to '%s'" outputDir

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
