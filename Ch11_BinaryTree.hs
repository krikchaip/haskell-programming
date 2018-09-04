module Ch11_BinaryTree where

data BTree a =
  Leaf | Sub (BTree a) a (BTree a)
  deriving (Eq, Ord, Show)

insert :: Ord a => a -> BTree a -> BTree a
insert x Leaf = Sub Leaf x Leaf
insert x (Sub l x' r)
  | x < x'    = Sub (insert x l) x' r
  | x > x'    = Sub l            x' (insert x r)
  | otherwise = Sub l            x' r

mapTree :: (a -> b) -> BTree a -> BTree b
mapTree _ Leaf = Leaf
mapTree f (Sub l x r) = Sub (mapTree f l) (f x) (mapTree f r)

testTree' :: BTree Integer
testTree' =
  let subleft  = Sub Leaf 3 Leaf
      subright = Sub Leaf 4 Leaf
  in  Sub subleft 1 subright

mapExpected :: BTree Integer
mapExpected =
  let subleft  = Sub Leaf 4 Leaf
      subright = Sub Leaf 5 Leaf
  in  Sub subleft 2 subright

-- acceptance test for mapTree
mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

preorder :: BTree a -> [a]
preorder Leaf        = []
preorder (Sub l x r) = [x] ++ preorder l ++ preorder r

inorder :: BTree a -> [a]
inorder Leaf        = []
inorder (Sub l x r) = inorder l ++ [x] ++ inorder r

postorder :: BTree a -> [a]
postorder Leaf        = []
postorder (Sub l x r) = postorder l ++ postorder r ++ [x]

testTree :: BTree Integer
testTree =
  let subleft  = Sub Leaf 1 Leaf
      subright = Sub Leaf 3 Leaf
  in  Sub subleft 2 subright

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- any traversal order is fine
type Reducer a b = a -> b -> b
foldTree :: Reducer a b -> b -> BTree a -> b
foldTree _ acc Leaf        = acc
foldTree f acc (Sub l x r) = foldTree f (f x leftResult) r
  where leftResult = foldTree f acc l
