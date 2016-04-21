import Data.Char
import Data.List
import Data.Ord

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving Show    
data Unit = Unit deriving Show

type Token = String
type Freq = Int
type Markov_Model = [(Token, [(Token, Freq)])]

-- main = mapM_ print $ get_markov_model $ tokenize $ map toLower input
main = print $ generate_text "WIllem" 10

generate_text :: Token -> Int -> [Token]
generate_text t_last 0 = [t_last]
generate_text t_last remaining = t_last : generate_text next_word (remaining-1)
	where
		next_word = "hoi"

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

input :: String
input = "The code includes definitions for 128 characters: most of these are the code printable characters of the alphabet such as abc, ABC, 123, and ?&!. There are also control characters that cannot be printed but instead control how text is processed, to start a new line for example. Those are in the left column in the code below. Most of the control characters are no longer used for their original purpose. There is no real formatting control (for bold or Italics, etc.) [2]"

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
	| isAlphaNum c			= (c:(fst $ get_word s), snd $ get_word s)
	| otherwise 			= ([], skip_non_alphanum s)

skip_non_alphanum :: String -> String
skip_non_alphanum "" = ""
skip_non_alphanum (c:s) 
	| isAlphaNum c	= c:s
	| otherwise 	= skip_non_alphanum s
