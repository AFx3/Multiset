-- Multiset: collection, elements can appear multiple times
-- Multiplicities: #times each element appears aka #occ

-- $: applies function to an argument, has very low precedence (right-associative): it is evaluated after other operators
-- f $ x = f x

-- fold': reduces a list to a single value by iteratively applying a binary function to an accumulator and the elements of the list
--        is a strict left fold, which means it forces the evaluation of the accumulator at each step

module MultiSet
( MSet         -- MS data type
, empty        -- function to create empty MS
, add          -- function to add an element to MS
, occs         -- function to return #occ of an element in MS
, elems        -- function to return list of all elements in MS (no multiplicities)
, subeq        -- function to check if a MS is a subMS of another: every element in the 1st must appear in the 2nd MS with at least the same multiplicity
, union        -- function to return a new MS that is the union of two MS. The multiplicity of each element in the union is the sum of its multiplicities in the original MS
, fromList     -- function to convert a list into MS. Each elem in the list will be an element of the MS with a multiplicity equal to its #occ in the list
, toString     -- function to convert a MS into a formatted string, where each element is listed along with its multiplicity
) where

import Data.List (foldl')

--------------------------------------------------------------------------
-- MULTISET DATA TYPE
-- 'MSet a': the Mset is parametrized by a. it can hol elems of any type a
-- it is a type storing list of tuples (a: element type, int: multiplicity) 
-- it generates an instance of Show typeclass for MSet a, so that it converts an Mset value to a string: calling show mySet otuputs its elemnts
data MSet a = MS [(a, Int)] deriving (Show)


--------------------------------------------------------------------------
-- EMPTY FUNCTION
--returns an empty MS

-- empty function returns a value of type MSet::a
-- construct an MS with an empty list: no elems	
empty :: MSet a
empty = MS []

--------------------------------------------------------------------------
-- ADD FUNCTION
-- If the element is already present, it increases its multipl by 1, elese inserted with multipl 1
-- Eq a => :the types a must belong to the Eq typecalss (elem a can be compared for equality). Used to check if an element v is already in the MS
-- MSet a -> a -> MSet a : function takes 2 args the MS 'Mset a' and an elem of type a to add. It returns a new Mset a (the updated MS) 
add :: Eq a => MSet a -> a -> MSet a 

-- (MS xs): pattern matching to extract list of tuples xs from MS. Each tup xs has form (v, occ) 
-- v: elem to add
-- MS $ go xs: construct new MS applying helper func go to the list xs
-- go helper func to traverse the list in the MS ad update it
add (MS xs) v = MS $ go xs 
  where
    -- Base case. Pattern xs is empty (go []), the result is a list containing 1 tup (v,1), meaning v is added to MS with multipl 1
    go [] = [(v, 1)] 
     
    -- Recursive case. Pattern non-empty list, x is 1st elem and occ its multipls, xs' the rest of the list     
    go ((x, occ):xs') 
    
      -- if current elem x == to elem v to add. So add new tup to the list increasing multipl by 1 (v already present), continute to remeninig elems xs'    
      | x == v    = (x, occ + 1) : xs'
      
      -- v not found yet, recurse to the rest of the list. Keep current tup (x,occ) as it is and recursively apply go to the rest of the list
      | otherwise = (x, occ) : go xs'  
      
{- example: MS [(3,2), (5,2)] add v=5
go [(3,2), (5,2)]
1st elem no matches 5 -> nisba
rec call: 2nd elem -> matches 5
update (5,3)
result: [(3,2), (5,3)]
-}

--------------------------------------------------------------------------
-- #OCC FUNCTION
-- evaluates the multipl of an element in the MS
-- type a must be instance od Eq type class for comparisons
-- arguments: a Mset (multiset) an element of type a (terget to find #occ)
-- returns an int (#occ)
occs :: Eq a => MSet a -> a -> Int
-- pattern matchning: the MS xs is constructor that contains list of tups (elem, #occ), v element to find multipl 
-- =
-- lookup function to search v in the list xs (lookup v xs). Returns a Maybe int: Just n: ff v is found, where n is #occ of v. Nothing: ff v is not found
-- so use maybe function to handle Mybe values: maybe :: b -> (a -> b) -> Maybe a -> b
-- 0: default value if lookpu ret Nothig
-- id: function to apply to value inside Just. id is the identity function, it returns its argument unchanged
occs (MS xs) v = maybe 0 id $ lookup v xs

{- example: element found MS [(3,2), (5,2)] find #occ of v=5
input occs (MS [(3,2), (5,2)]) %
lookup finds 5 in the list, returns Just 2
maybe 0 id (Just 2)
id applied to 2 returns 2
result: 2
--
element not found: 
lookup v=4, returns Nothing
maybe 0 id Nothing, maybe returns default value 0
result: 2
-}

--------------------------------------------------------------------------
-- LIST OF ALL ELEMENTS (NO MULTIPLICITIES)
-- elems returns a list of all elements in the multiset, with no multpl
elems :: MSet a -> [a]
-- pattern matching (MS xs) is the pattern that matches the MS type, xs says MS contains list of tups
-- =
-- map: function to apply another function to each elems of the list
-- fst: function taking a tup and ret its 1st elem
elems (MS xs) = map fst xs

{- example: input elems (MS [(3, 2), (5, 2)])
pattern matching: xs is [(3, 2), (5, 1)]
map fst xs: apply fst to each tup
fst (3, 2) -> 3
fst (5, 1) -> 5
rsult: [3,5]
-}

--------------------------------------------------------------------------
-- CHECK A MS IS A SUBSET OF ANOTHER
-- it verifies if every element in the 1st MS appears in the 2nd MS with at least the same multiplicity
-- type a support equality comp, args 2 MS to be compared, returns a boolean
subeq :: Eq a => MSet a -> MSet a -> Bool
-- pattern matching (MS xs) (MS ys) are the patterns that matche the MS type, xs/ys say each MS contains list of tups
-- =
-- all: checks if a predicate holds true for all elements in the list, returns true/false
-- \(x, occ): lambda func taking the tuple
-- lookup x ys: find  #occ of x in the 2nd MS. Returns Just n if x is found in ys with multipl n, Nothing if x is not found in ys
-- maybe False (>= occ): handles the result from lookup, converts nothing to False (x not present in ys) and applies the funct (>= occ) if the result is Just n
-- (>= occ): If lookup ret Just n, checks if #occ n is >=  to occ. If true, the predicate is satisfied for this (x, occ).
subeq (MS xs) (MS ys) = all (\(x, occ) -> maybe False (>= occ) (lookup x ys)) xs

{- example: check if mset1 is a subMS of mset2
mset1 = MS [(3, 2), (5, 1)]
mset2 = MS [(3, 3), (5, 2), (7, 1)]
pattern matching: subeq (MS [(3, 2), (5, 1)]) (MS [(3, 3), (5, 2), (7, 1)])
process with all:
for (3, 2) in mset1:
	lookup 3 [(3, 3), (5, 2), (7, 1)] returns Just 3
	maybe False (>= 2) (Just 3) evals True  (3 >= 2)
For (5, 1) in mset1:
	lookup 5 [(3, 3), (5, 2), (7, 1)] returns Just 2
	maybe False (>= 1) (Just 2) evals True (2 >= 1)
-}

--------------------------------------------------------------------------
-- UNION OF 2 MULTISETS
-- it combines 2 MS into a new MS where multiplicity of each elem is the sum of its multipl in the original MS
-- also here type a must support eq comparison. it takes 2 Msets of type a and returns a MS of type a
union :: Eq a => MSet a -> MSet a -> MSet a

-- match types of the two MS, xs and ys lists of tups
-- =
-- foldl': processes the list from left to right
-- addPair: function applied to each element of xs, accumulating results in ys
-- (foldl' addPair ys xs processes each (x, occ) from xs and applies addPair to accumulate the results)
-- the result is passed to MS
union (MS xs) (MS ys) = MS $ foldl' addPair ys xs
  where
  -- acc is the accumulator, start as ys ans is updated at each iteration, (x, occ) is the currein tup from xs
  -- and check if x is already in the acc. It returns Just occ' if x is present with multipl occ', Nothing if x is not in acc.
    addPair acc (x, occ) = case lookup x acc of
    -- if x is already in the acc, update its multipl by adding occ to occ', with replace updates the acc.
      Just occ' -> replace x (occ + occ') acc
      -- if x is not in the acc, add (x, occ) to acc
      Nothing   -> (x, occ) : acc
-- update the multipl of x in the list
-- need to do:
-- base case: if the list is empty (i.e. nothing to replace), return an empty list ignoring the first 2 elem when list is empty
-- inductive case: if x == y: Replace (y, occ) with (y, n) and n is the updated multiplicity. Else Keep current tup (y, occ), recursively process rest of list ys'

--empty list
    replace _ _ [] = []
    
-- pattern matching to get the head ((y, occ)) and the tail (ys') of list
-- x: element trying to be replaced, n is its new multipl
-- y: current element in the list, and occ is its current multipl 
    replace x n ((y, occ):ys')
    -- if current elem y matches elem x that want to replace -> replace (y, occ) with (y, n) and return the new list
    -- (y, n): updated tuple with new multiplicity n, followed by the rest of list ys'
      | x == y    = (y, n) : ys'
      -- else -> keep the current (y, occ) as it is and recursively call replace on the rest of the list ys'
      --(searches thru the list until x is found or the list is completely passed)
      | otherwise = (y, occ) : replace x n ys'
      
--------------------------------------------------------------------------      

-- CONVERT A LIST INTO A MS
-- it takes a list of elements of type a and returns an MSet a, ensure elements of type a can be compared
fromList :: Eq a => [a] -> MSet a

-- function add is applied to each element of the list, start with an empty MS (empty) and adds each elem in the list to the MS
-- by creating a MS that has the multipl of the elements in the list. The list is prcessed form l to r 
fromList = foldl' add empty

--------------------------------------------------------------------------
--CONVERT A MS INTO A FORMATTED STRING

-- tkes a MS of type a, returns a String. Use Show to ensure that elems of type a can be converted to a string using using show
toString :: Show a => MSet a -> String

{- pattern matching to get the list xs from the MS xs
-- map applies the lambda func to each (x, n) in `xs`
-- lambda f converts each elem and its multiplicity into string as follows: "<element> - <multiplicity>"
-- use unlines to after join these strings into one string, separating by newlines
-}
toString (MS xs) = unlines $ map (\(x, n) -> "<" ++ show x ++ "> - <" ++ show n ++ ">") xs

--------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------
-- FUNCTION TO APPLY A FUNCTION TO ALL ELEMS OF A MS
-- it takes a function (a -> b) that transforms an element of type a to type b and a MS of type a
-- it returns a MS of type b
-- elements of type `b` can be compared for equality
mapMSet :: Eq b => (a -> b) -> MSet a -> MSet b

{- pattern match extracts the list x` from the MS xs
concatMap (\(x, n) -> replicate n (f x)) xs: applies the function f to each elem x in the MS and replicates it n-times (where n is the multipl of x)
use concatMap to concatenate lists of transformed elements into a single list
fromList: to convert this list back into a MSet b
-}
mapMSet f (MS xs) = fromList $ concatMap (\(x, n) -> replicate n (f x)) xs

--------------------------------------------------------------------------
-- Instance definitions
--------------------------------------------------------------------------
-- TWO MSs ARE EQUAIL IF THEY HAVE SAME ELEMENTS WITH SAME MULTIPLICITY
-- define an instance of Eq typeclass for MSet a
instance Eq a => Eq (MSet a) where

-- compares two MSs by checking if all elements in xs are in ys and vice versa
  MS xs == MS ys = all (`elem` ys) xs && all (`elem` xs) ys
  
 --------------------------------------------------------------------------
-- FOLD A MS IGNORING MULTIPLICITIES
-- define an instance of the Foldable typeclass for MSet
instance Foldable MSet where

-- foldr applies a function to each element in the MS, starting with an accumulator
-- if the MS is empty (MS []) -> the result is the accumulator
  foldr f acc (MS []) = acc

-- if MS is not empty, pattern matching extracts 1st elem x (and its multipl, ignored) from the list, and xs is the rest of the list
-- f x is applied to x and then foldr recursively called on the rest of xs
  foldr f acc (MS ((x, _):xs)) = f x (foldr f acc (MS xs))
  
--------------------------------------------------------------------------  
--------------------------------------------------------------------------
-- FUCNTION TO REMOVE DUPLICATES FROM A LIST
-- takes a list and returns a list of type a and those types must be comparable 
rmdup :: Eq a => [a] -> [a]

-- rmdup is defined using foldl'
-- lambda f checks if current element x is already in the accumulator list 
-- If x is in acc, the accumulator remains the same
-- else, x is appended to acc
-- initial val of accumulator is the empty list '[ ]'
rmdup = foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) []
--------------------------------------------------------------------------
-- FUNCTION TO COMBINE ELEMS IN A LIST USING A GIVEN FUNCTION REMOVIND DUPLICATES
-- takes a list of tups, a function ([b] -> b) that combines a list of b values into a single b value
-- returns a list of tuples 
--  lements of type a, b can be compared
reduce :: (Eq a, Eq b) => [(a, b)] -> ([b] -> b) -> [(a, b)]

{- 
first maps each tup (x, _) to a new tuple (x, combined_value), 
where combined_value is the result of applying the unify func to a list of all b values associated with x in xs
filter ((== x) . fst): is used to filter xs to keep only those tups having 1st element matching x
map snd: extracts the s2nd elem (the b value) from each filtered tuple
unify then combines the b values into a single b value
and rmdup removes any duplicate tuples from the result
-}
reduce xs unify = rmdup $ map (\(x, _) -> (x, unify $ map snd $ filter ((== x) . fst) xs)) xs



