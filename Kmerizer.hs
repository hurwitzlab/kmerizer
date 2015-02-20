{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Bio.Core.Sequence 
import Bio.Sequence.Fasta 
import Data.List
import System.Console.CmdArgs
import Text.Printf (printf)
import Data.Foldable (forM_)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

-- # --------------------------------------------------
data Options = Options { 
    inputFile :: String, 
    outDir    :: String,
    kmerSize  :: Int 
} deriving (Data, Typeable, Show)

options = Options { 
      inputFile = ""  &= typ "FILE" &= help "Input file"
    , outDir    = "." &= typ "DIR"  &= help "Output directory"
    , kmerSize  = 20               &= help "k-mer size (20)"
 } 
 &= summary "create k-mer/location files for a given FASTA file"
 &= program "kmerizer"

-- # --------------------------------------------------
kmerize :: BioSeq a => Int -> a -> ([Char],[[Char]])
kmerize n seq =
    ( BC.unpack $ unSL $ seqid seq,
      findKmers n $ BC.unpack $ unSD $ seqdata seq
    )

-- # --------------------------------------------------
findKmers :: Int -> [Char] -> [[Char]]
findKmers n = filter (\s -> length s == n) . map (take n) . tails

-- # --------------------------------------------------
main = do
    opts  <- cmdArgs $ options
    inFile <- inputFile opts
    print inFile 
    input <- readFasta inFile
    -- let outdir = outDir opts
    -- print outdir ++ inFile

    -- let kmers = map (kmerize (kmerSize opts)) input

    {-
    let counts = map (\(id, k) -> id ++ "\t" ++ show (length k)) kmers
    mapM_ putStrLn counts

    let mers = map (\(i,k) -> show i ++ "\t" ++ k) $ zip [1..] (concat $ map (\(id, k) -> k) kmers) 
    mapM_ putStrLn mers

    forM_ kmers $ \(i, k) ->
       putStrLn $ printf "%s\t%d" i (length k)
    -}

    -- forM_ (zip [1..] (concatMap snd kmers)) $
    --     putStrLn . uncurry (printf "%d\t%s")

    putStrLn "done"
