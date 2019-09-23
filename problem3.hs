
import System.Environment

import System.IO
import Data.List
import Data.List.Split
import Debug.Trace

import qualified Data.Text as Tx


data Parameters = Parameters {
  count_n :: Int,
  seq_l :: Int,
  max_d :: Int
  } deriving Show


data CandidateSequence = CandidateSequence {
  s_sequence :: String,
  s_index :: Int,
  s_score :: Int
  } deriving Show

instance Eq CandidateSequence where
  CandidateSequence a b c == CandidateSequence a' b' c' = a == a'

createParams :: String -> Parameters
createParams input =
  Parameters n l d
  where [n, l, d] = [read x :: Int | x <- splitOn " " input]

main :: IO()
main = getArgs >>= mapM_ execute


execute :: String -> IO()
execute filepath = do
  content <- openFile filepath ReadMode >>= hGetContents
  let flines = Data.List.filter (not . null) (splitOn "\n" content)
  mapM_ putStrLn flines

  let z = trace (show $ length flines) (1)

  let w = processSequence flines
  putStrLn $ show w

processSequence :: [String] -> [[(String, Bool)]]
processSequence inputs = evaluated_substrings
  where
    parameters = createParams $ inputs !! 0
    genome = inputs !! 1
    available_positions = [0..(length genome - seq_l parameters -1)]
    candidateset = nub [(makeCandidate genome i (i + seq_l parameters)) | i <- available_positions]
    evaluated_substrings = [[(evaluateMatch parameters genome candidate genomic_index) | genomic_index <- available_positions] | candidate <- candidateset]


--evaluateSubstring :: String -> String
--evaluateSubstring genome substring =
--  map genome

evaluateMatch :: Parameters -> String -> CandidateSequence -> Int -> (String, Bool)
evaluateMatch parameters genome candidateseq genomic_index =
  all_positions
  where
    all_positions = (\seq -> evalmatch' parameters genome seq "" genomic_index 0) candidateseq


evalmatch' :: Parameters -> String -> CandidateSequence -> String -> Int -> Int -> (String, Bool)
evalmatch' parameters genome candidateseq buffer genomic_index current_index =
  case allowed_score of
    True -> case or [end_of_run_candidate, end_of_run_genome] of
      True -> (buffer, True)
      False -> evalmatch' parameters genome candidateseq (buffer ++ match) genomic_index $ current_index + 1
    False -> (buffer, False)
  where
    allowed_score = s_score candidateseq <= max_d parameters

    zz = trace ((show current_index) ++ " " ++ (show genomic_index)) (cidx)
    cidx = trace(show current_index) (1)

    end_of_run_candidate = current_index + zz == (length $ s_sequence candidateseq)
    end_of_run_genome = (length genome) + zz == (genomic_index + current_index)

    get_genomebase = \w -> (genome ++ "q") !! (genomic_index + current_index + w)
    get_candidatebase = \w -> ((s_sequence candidateseq) ++ "z") !! (current_index + w)

    match = case get_genomebase 0 == get_candidatebase 0 of
      True -> "M"
      False -> case get_genomebase 1 == get_candidatebase 0 of
        True -> "I"
        False -> case get_genomebase 0 == get_candidatebase 1 of
          True -> "D"
          False -> "X"


removeMisfitSubstrings :: Parameters -> [[(Int, String)]] -> [[(Int, String)]]
removeMisfitSubstrings parameters inputs = positive_matches
  where
    positive_submatches = [filter (\(score, string) -> score < max_d parameters) submatches | submatches <- inputs]
    positive_matches = filter (\w -> length w >= count_n parameters) positive_submatches


makeSubstring :: String -> Int -> Int -> String
makeSubstring source start end =
  take (end - start) (drop start source)

makeCandidate :: String -> Int -> Int -> CandidateSequence
makeCandidate genome s_pos s_len =
  CandidateSequence (makeSubstring genome s_pos $ s_pos + s_len) s_pos $ -1
  
parseCigarString :: String -> String
parseCigarString input = pcsloop' "" 0 "" input

pcsloop' :: String -> Int -> String -> String -> String
pcsloop' output buffer lastc remaining =
  case length remaining > 0 of
    True -> pcsloop' nextoutput nextbuffer nextc nexts
    False -> nextoutput
  where
    nextc = take 1 remaining
    nexts = drop 1 remaining
    continue_chain = nextc == lastc
    nextoutput = case continue_chain of
      True -> output
      False -> case buffer of
        0 -> output
        _ -> output ++ (show buffer) ++ lastc
    nextbuffer = case continue_chain of
      True -> buffer + 1
      False -> 1
