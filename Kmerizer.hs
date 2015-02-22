{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Bio.Core.Sequence 
import Bio.Sequence.Fasta 
import Data.Foldable (forM_)
import Data.List (tails)
import Data.Stringable (toString)
import System.Console.CmdArgs
import System.IO 
import System.FilePath.Posix (joinPath, takeBaseName)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

-- # --------------------------------------------------
data Options = Options { 
    inputFile :: Maybe String, 
    outDir    :: String,
    kmerSize  :: Int 
} deriving (Data, Typeable, Show)

options = Options { 
      inputFile = ""  &= typ "FILE" &= help "Input file"
    , outDir    = "." &= typ "DIR"  &= help "Output directory"
    , kmerSize  = 20                &= help "k-mer size (20)"
 } 
 &= summary "create k-mer/location files for a given FASTA file"
 &= program "kmerizer"

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
main = do
    opts  <- cmdArgs options

    let inFile       = inputFile opts
    let outputDir    = outDir opts
    let baseName     = takeBaseName inFile
    let outFileKmers = joinPath [outputDir, baseName ++ ".kmers"]
    let outFileLoc   = joinPath [outputDir, baseName ++ ".locs"]

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

    putStrLn "done"
