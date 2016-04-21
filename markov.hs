-- set makeprg=ghc\ %
import Data.Char
import Data.List
import Data.Ord
import Data.Maybe
import Data.Function(on)
import Debug.Trace

import System.Random

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving Show    
data Unit = Unit deriving Show

type Token = String
type Freq = Int
type Markov_Model = [(Token, [(Token, Freq)])]
type RandomInts = [Int]

main = do 
	let rand_gen = mkStdGen 4321
	input_string <- readFile "lamacollectief.txt"
	print $ unwords $ generate_text (model input_string) "ik" 1000 rand_gen
		where 
			model text = get_markov_model $ tokenize $ map toLower text

generate_text :: Markov_Model -> Token -> Int -> StdGen -> [Token]
generate_text model t_last 0 _  = [t_last]
generate_text model t_last remaining rand_gen
	| isJust (model_entry) = t_last : generate_text model next_token (remaining-1) new_rand_gen
	| otherwise = ["!!!EOF: NOTHING FOUND"]
		where
			model_entry :: Maybe (Token, [(Token, Freq)])
			model_entry = find (\(a, b) -> a==t_last) model
			(next_token, new_rand_gen) = next_word model t_last rand_gen
			-- rand_word :: (Token, [(Token, Freq)]) -> Token
			-- rand_word entry = fst $ (!!) (snd entry) (mod random (length (snd entry)))
			-- random :: Int
			-- random = (random_numbers !! (remaining `mod` ( length random_numbers)) )

-- type Markov_Model = [(Token, [(Token, Freq)])]
next_word :: Markov_Model -> Token -> StdGen -> (Token, StdGen)
next_word _ "" _  = error "got empty token"
next_word model prev rand_gen
	| isJust(m_followups) = (fair_word $ fromJust m_followups, new_rand_gen)
	| otherwise = ("!EOF: NOTHING FOUND!", new_rand_gen)
	where
		m_followups :: Maybe [(Token, Freq)]
		m_followups = lookup prev model
		best_word :: (Token, [(Token, Freq)]) -> Token
		best_word entry = fst $ maximumBy (compare `on` snd) $ snd entry
		(rand_num, new_rand_gen) = next rand_gen
		fair_word :: [(Token, Freq)] -> Token
		fair_word selection = find_entry_on_scale selection $ rand_num

find_entry_on_scale :: [(Token, Freq)] -> Int -> Token
find_entry_on_scale ts num = fst $ result  -- trace ("rand: "++show (max_prob) ++" --- " ++  show(result) ++ " ----------" ++show(accum_prob_map))   
	where
		result :: (Token, Freq)
		result = head $ filter (\(t,f) -> (num `mod` (max_prob+1)) <= f ) (accum_prob_map)
		-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
		(max_prob, accum_prob_map) = mapAccumL (\ a (t,f) -> (a+f, (t,a+f)) ) 0 ts

get_markov_model :: [Token] -> Markov_Model
get_markov_model ts = 
	map ( \a -> 
			(a, fol_freq $ followups a)
		) $ nub ts
	where
		pairs :: [(Token, Token)]
		pairs = get_token_pairs ts
		followups :: Token -> [Token]
		followups token = map snd $ filter (\(a, b) -> a==token) pairs
		fol_freq :: [Token] -> [(Token, Freq)]
		fol_freq ts = map (\t -> (head t, length t)) $ group $ sort ts

token_frequencies :: [Token] -> [(Token, Freq)]
token_frequencies ts = map (\t -> (head t, length t)) $ group $ sort ts

get_token_pairs :: [Token] -> [(Token, Token)]
get_token_pairs [] = []
get_token_pairs (t:[]) = [(t, "EOF")]
get_token_pairs (t:ts) = (t, head ts) : get_token_pairs ts

tokenize :: String -> [Token]
tokenize [] = []
tokenize s = first_word : ( tokenize remainder )
	where (first_word, remainder) = get_word s

get_word :: String -> (Token, String)
get_word "" = ("","")
get_word (c:[]) = ([c], [])
get_word (c:s) 
	| isSpace c = get_word s --Skip spaces, newlines, returns etc...
	| isPunctuation c = ([c], s)
	| otherwise 	  = (c:rest_of_word, remainder)
	where (rest_of_word, remainder) = break (\x -> x /= '\'' && isPunctuation x || isSpace x) s

skip_non_alphanum :: String -> String
skip_non_alphanum "" = ""
skip_non_alphanum (c:s) 
	| isAlphaNum c	= c:s
	| otherwise 	= skip_non_alphanum s
