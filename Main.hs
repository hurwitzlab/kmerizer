module Main where

import Bio.Core.Sequence
import Bio.Sequence.Fasta
import Control.Monad(unless)
import Data.Foldable(forM_)
import Data.Monoid
import Data.Stringable(toString)
import Options.Applicative
import System.FilePath.Posix(joinPath, takeBaseName)
import System.Directory
import System.IO
import Text.Printf(printf)

-- # --------------------------------------------------
data Options = Options {
  inputFile :: String,
  outDir    :: String,
  kmerSize  :: Int
} deriving (Show)

-- # --------------------------------------------------
kmerize :: BioSeq a => Int -> a -> (String, Int, [String])
kmerize n seqRead = (seqID, numKmers, kmers)
 where seqID = toString . seqid $ seqRead
       (numKmers, kmers) = findKmers n . toString . seqdata $ seqRead

-- # --------------------------------------------------
findKmers :: Int -> String -> (Int, [String])
findKmers k xs = (n, findKmers' n k xs)
 where n = length xs - k + 1
       findKmers' n' k' xs'
         | n' > 0 = take k' xs' : findKmers' (n' - 1) k' (tail xs')
         | otherwise = []

-- # --------------------------------------------------
runWithOptions :: Options -> IO ()
runWithOptions opts = do
  let inFile       = inputFile opts
  let outputDir    = outDir opts
  let baseName     = takeBaseName inFile
  let outFileKmers = joinPath [outputDir, baseName ++ ".kmers"]
  let outFileLoc   = joinPath [outputDir, baseName ++ ".loc"]

  outdirExists <- doesDirectoryExist outputDir
  unless outdirExists (createDirectory outputDir)

  input <- readFasta inFile
  let kmers = map (kmerize (kmerSize opts)) input

  locFh  <- openFile outFileLoc   WriteMode
  kmerFh <- openFile outFileKmers WriteMode

  forM_ kmers $ \(readId, numKmers, readKmers) -> do
    hPutStrLn locFh $ printf "%s\t%d" readId numKmers
    mapM_ (hPutStrLn kmerFh) readKmers

  hClose locFh
  hClose kmerFh

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
