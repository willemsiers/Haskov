-- set makeprg=ghc\ %
import Data.Char
import Data.List
import Data.Ord
import Data.Maybe
import Data.Function(on)
import Debug.Trace
import System.Environment
import System.Exit

import System.Random

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving Show    
data Unit = Unit deriving Show

type Token = String
type Freq = Int
type Followup = (Token, Freq)
type Gram2 = (Token, Token)
type Gram3 = (Token, Token, Token)
type Markov_Model = ([(Gram3, [Followup])], [(Gram2, [Followup])], [(Token, [Followup])])

main = do 
	let rand_gen = mkStdGen 4321
	prog_args <- getArgs
	(filename, initial_word, word_count) <- (handleArgs prog_args)

	input_string <- readFile filename
	print $ unwords $ generate_text (model input_string) [initial_word] word_count rand_gen
		where 
			model text = get_markov_model $ tokenize $ map toLower text

	
	--	return = Just
	--	fail _ = Nothing
-- handleArgs :: [String] -> Maybe (String, String)
handleArgs [] = do 
	fail "\nUsage: markov FILE [WORD] [COUNT]"
handleArgs (a:[]) = do 
	return (a, "the", 500)
handleArgs [filename, initial_word] = do 
	return (filename, initial_word, 500) 
handleArgs [filename, initial_word, count] = do 
	return (filename, initial_word, read count :: Int) 

-- Repeatedly calls next_word
generate_text :: Markov_Model -> [Token] -> Int -> StdGen -> [Token]
generate_text _ _ 0 _  = []
generate_text _ [] _ _  = error "generate_text: history is empty ! This should never happen"
generate_text model history remaining rand_gen = next_token : generate_text model (history++[next_token]) (remaining-1) new_rand_gen
		where
			(next_token, new_rand_gen) = next_word model history rand_gen

-- type Markov_Model = ([(Gram3, [Followup])], [(Gram2, [Followup])], [Token])
-- Try match as long a number from the "history" [Token] list with the Markov_Model. First try 3, then 2 then 1. Return a matched (semi-randomized/weighted) Token as well as a new StdGen
next_word :: Markov_Model -> [Token] -> StdGen -> (Token, StdGen)
next_word _ [] _  = error " next_word: history is empty! This should never happen."
next_word model history rand_gen 
	| length history >= 3 && isJust(m_followups 3) = (fair_match $ fromJust (m_followups 3), new_rand_gen)
	| length history >= 2 && isJust(m_followups 2) = (fair_match $ fromJust (m_followups 2), new_rand_gen)
	| length history >= 1 && isJust(m_followups 1) = (fair_match $ fromJust (m_followups 1), new_rand_gen)
	| otherwise = ("!EOF: NOThING FOUnD 2!", new_rand_gen)
	where
		m_followups :: Int -> Maybe [Followup]
		m_followups lookback
			| lookback == 1 = lookup last_one (modeltokens model)
			| lookback == 2 = lookup last_two (model2grams model)
			| lookback >= 3 = lookup last_three (model3grams model)
			| otherwise = error "uncaught next_word case"
		fair_match :: [Followup] -> Token
		fair_match selection = find_entry_on_scale selection $ rand_num
		last_one :: Token
		last_one = (last history)
		last_two :: Gram2
		last_two = (last.init $ history, last history)
		last_three :: Gram3
		last_three = (last.init.init $ history, last.init $ history, last history)
		(rand_num, new_rand_gen) = next rand_gen
		-- best_word :: (Token, [Followup]) -> Token
		-- best_word entry = fst $ maximumBy (compare `on` snd) $ snd entry

-- Get the (Token, [Followup]) pairs from the model
modeltokens :: Markov_Model -> [(Token, [Followup])]
modeltokens (_, _, a) = a
model2grams :: Markov_Model -> [(Gram2, [Followup])]
model2grams (_, a, _) = a
model3grams :: Markov_Model -> [(Gram3, [Followup])]
model3grams (a, _, _) = a

-- Picks a (random) element from [Followup] based on it's frequency. The second arguments should be random for a random pick from the weighted Followups.
find_entry_on_scale :: [Followup] -> Int -> Token
find_entry_on_scale ts num = fst $ result  -- trace ("rand: "++show (max_prob) ++" --- " ++  show(result) ++ " ----------" ++show(accum_prob_map))   
	where
		result :: Followup
		result = head $ filter (\(t,f) -> (num `mod` (max_prob+1)) <= f ) (accum_prob_map)
		-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
		(max_prob, accum_prob_map) = mapAccumL (\ a (t,f) -> (a+f, (t,a+f)) ) 0 ts

-- type Markov_Model = ([(Gram3, [Followup])], [(Gram2, [Followup])], [(Token, [Followup])])
-- TODO: FIll in gram3 and gram2
get_markov_model :: [Token] -> Markov_Model
get_markov_model ts = 
	([],
	 [],
	map ( \a -> 
			(a, fol_freq $ followups a)
		) $ nub ts
	)
	where
		pairs :: [Gram2]
		pairs = get_token_pairs ts
		followups :: Token -> [Token]
		followups token = map snd $ filter (\(a, b) -> a==token) pairs
		fol_freq :: [Token] -> [Followup]
		fol_freq ts = map (\t -> (head t, length t)) $ group $ sort ts

token_frequencies :: [Token] -> [Followup]
token_frequencies ts = map (\t -> (head t, length t)) $ group $ sort ts

get_token_pairs :: [Token] -> [Gram2]
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
