import MultiSet
import Data.List (sort)
import Data.Char (toLower)

in_dir = "./aux_files/"

-- `ciao s` returns a string that contains all characters of s in lowercase and sorted alphabetically
ciao :: [Char] -> [Char] -- type signature of ciao: it takes a list of chars and returns a list of chars
ciao s = sort (map toLower s)  -- implement the ciao function: convert s to lowercase and sort the chars

-------------------------------------------------------------------------------------
-- READ MS
-- readMSet filename reads a file whose name is filename (passed as arg, as a string) 
-- returns an MS having the ciao of all the words of the file with the corresponding mutiplicity, so:

-- readFile filename: reads the content of the file as a single string
-- bind operator is used to sequence the IO actions, it takes the res of readFile and passes it to the lambda f
-- lmbda f: processes the file content to create the MS, and return ensures the result is in the IO monad
-- words x: splits the file content into a list of strings, x is the content of the file
-- map ciao: applies the ciao function to each word, converting them to lowercase and sorts their chars
-- fromList: converts the list of "ciao"-ed words into a MS, where each unique word is stored with its multiplicity
-- return: wraps the result in the IO monad
readMSet :: String -> IO (MSet String)
readMSet filename = readFile filename >>= (\x -> return $ fromList $ map ciao $ words x)

-------------------------------------------------------------------------------------
-- WRITE MS
-- writeMSet: given a multiset and a file name, writes in the file, one per line, each element of the multiset with its multiplicity in the format “<elem> - <multiplicity>”

-- define type signature of writeMSet: takes a MS and a filename, returns an IO action that produces no value (i.e., `()`)
writeMSet :: Show a => MSet a -> String -> IO () 

-- toString mset:  converts the MS into a formatted string
-- writeFile filename to writes the string to the specified file
writeMSet mset filename = writeFile filename $ toString mset

-------------------------------------------------------------------------------------
-- MAIN IO
-- Using readMSet, from directory aux_files it loads files anagram.txt, anagram_s1.txt, anagram_s2.txt and margana2.txt in corresponding multisets, that we call m1, m2, m3 and m4 respectively;
-- Exploiting also the functions imported from MultiSet.hs, it checks the following facts and prints a corresponding comment:
--      i. Multisets m1 and m4 are not equal, but they have the same elements;
--     ii. Multiset m1 is equal to the union of multisets m2 and m3;
-- Finally, using writeMSet it writes multisets m1 and m4 to files anag-out.txt and gana-out.txt, respectively.

-- define main function: the entry point of the program, it returns an IO action
main :: IO()
-- begin a block of IO actions
main = do
  -- load test files into four MS
  m1 <- readMSet (in_dir ++ "anagram.txt")
  m2 <- readMSet (in_dir ++ "anagram-s1.txt")
  m3 <- readMSet (in_dir ++ "anagram-s2.txt")
  m4 <- readMSet (in_dir ++ "margana2.txt")

  -- checks m1 and m4 are not equal, but have the same elements
  if (m1 /= m4) && ((sort $ elems m1) == (sort $ elems m4))
    then putStrLn "Multisets m1 and m4 are not equal, but they have the same elements"
    else putStrLn "ERROR: Multisets m1 and m4 are not equal, but they have the same elements"

  -- Checks that m1 is equal to the union of multisets m2 and m3
  if (m1 == (union m2 m3))
    then putStrLn "Multiset m1 is equal to the union of multisets m2 and m3"
    else putStrLn "ERROR: Multiset m1 is equal to the union of multisets m2 and m3"

  -- writes m1 and m4 to output files anag-out.txt and gana-out.txt
  writeMSet m1 "anag-out.txt"
  writeMSet m4 "gana-out.txt"
