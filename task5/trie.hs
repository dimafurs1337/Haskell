import Data.Map (Map, empty, findWithDefault, insertWith, insert, member, (!))
data Character = EmptyCharacter | Character Char deriving (Show, Eq, Ord)

data TrieNode = TrieNode { character :: Character
                         , keyCount :: Int
                         , children :: Map Character TrieNode } deriving (Show)

data Trie = Trie { head :: TrieNode } deriving (Show)

findFirstIndexOfAnyHelper :: [Char] -> String -> Int -> Maybe Int
findFirstIndexOfAnyHelper _ [] _ = Nothing
findFirstIndexOfAnyHelper chars (x:xs) currentIndex = if elem x chars
                                                        then Just currentIndex
                                                        else findFirstIndexOfAnyHelper chars xs $currentIndex + 1

findFirstIndexOfAny :: [Char] -> String -> Maybe Int
findFirstIndexOfAny chars a = findFirstIndexOfAnyHelper chars a 0

splitBy :: [Char] -> String -> [String]
splitBy separators s = filter (not . null) (splitByHelper separators s)
                        where splitByHelper separators s = case separatorIndex of
                                                            Just a -> (take a s) : splitBy separators (drop (a + 1) s)
                                                            Nothing -> [s]
                                                            where separatorIndex = (findFirstIndexOfAny separators s)

trieAdd :: TrieNode -> String -> TrieNode
trieAdd node (x:[]) = TrieNode (character node) (keyCount node) 
                                    (insertWith 
                                        (\_ oldValue -> (TrieNode (character oldValue) ((keyCount oldValue) + 1) (children oldValue)))
                                                            (Character x) (TrieNode (Character x) 1 empty) (children node))
trieAdd (TrieNode character keyCount children) (x:xs) = TrieNode character keyCount newChildren
                                                            where xCharacter = Character x
                                                                  xNode = trieAdd (findWithDefault (TrieNode xCharacter 0 empty) xCharacter children) xs
                                                                  newChildren = insert xCharacter xNode children              

trieBuild :: String -> Trie
trieBuild s = Trie (foldl trieAdd (TrieNode EmptyCharacter 0 empty) (splitBy [' '] s))

trieGetHelper :: TrieNode -> String -> Int
trieGetHelper node (x:[]) = let xCharacter = Character x
                                nodeChildren = children node
                            in
                                if member xCharacter nodeChildren
                                then keyCount (nodeChildren ! xCharacter)
                                else 0  
trieGetHelper (TrieNode _ _ children) (x:xs) = let xCharacter = Character x
                                                              in 
                                                                  if member xCharacter children
                                                                  then trieGetHelper (children ! xCharacter) xs
                                                                  else 0

trieGet :: Trie -> String -> Int
trieGet (Trie node) s = trieGetHelper node s