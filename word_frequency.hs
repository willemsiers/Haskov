import Data.Char
import Data.List
import Data.Ord

main = print 
	$ reverse 
	$ sortBy (comparing snd) 
	$ token_frequencies 
	$ tokenize input

input :: String
input = "The code includes definitions for 128 characters: most of these are the printable characters of the alphabet such as abc, ABC, 123, and ?&!. There are also control characters that cannot be printed but instead control how text is processed, to start a new line for example. Those are in the left column in the table below. Most of the control characters are no longer used for their original purpose. There is no real formatting control (for bold or Italics, etc.) [2]"

token_frequencies :: [String] -> [(String, Int)]
token_frequencies ts = map (\t -> (head t, length t)) $ group $ sort ts

tokenize :: String -> [String]
tokenize [] = []
tokenize s = first_word : ( tokenize remainder )
	where (first_word, remainder) = get_word s

get_word :: String -> (String, String)
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
