x -: f = f x

data Color = Red | Black deriving(Eq,Show)
data Tree x = 
    Node x Color (Tree x) (Tree x)
  | Empty
  deriving(Show)
leaf x = Node x Red Empty Empty

data Direction = Lft | Rght deriving(Eq,Show)
--TreeZipper 1. other not chosen tree 2. direction of the chosen tree
data TreeZipper a = TreeZipper [(a, Color, Tree a, Direction)] deriving(Show)
newZipper = TreeZipper []

data TreeFocus a = TreeFocus (Tree a) (TreeZipper a) | Null deriving(Show)
newFocus tree = TreeFocus tree newZipper
updateValue value (TreeFocus (Node _ color left right) zipper) = (TreeFocus (Node value color left right) zipper)
updateColor color (TreeFocus (Node x _ left right) zipper) = (TreeFocus (Node x color left right) zipper)
getValue (TreeFocus (Node x color left right) zipper) = x
goLeft  (TreeFocus (Node x color left right) (TreeZipper crumbs)) = TreeFocus left  (TreeZipper $ (x, color, right, Lft):crumbs)
goRight (TreeFocus (Node x color left right) (TreeZipper crumbs)) = TreeFocus right (TreeZipper $ (x, color, left, Rght):crumbs)

goUp (TreeFocus currentBranch (TreeZipper ((value, color, otherBranch, direction):crumbs) ))
  | direction == Lft =  TreeFocus (Node value color currentBranch otherBranch) (TreeZipper crumbs)
  | direction == Rght = TreeFocus (Node value color otherBranch currentBranch) (TreeZipper crumbs)
goTop focus@(TreeFocus tree (TreeZipper crumbs))
  | length crumbs == 0 = focus
  | otherwise = goTop $ goUp focus

find value (TreeFocus Empty zipper) = Null
find value focus@(TreeFocus (Node x color left right) zipper)
  | value == x = focus --value already exists
  | value < x = find value $ goLeft focus
  | value > x = find value $ goRight focus

minimum' focus@(TreeFocus (Node x color Empty _) zipper) = focus
minimum' focus@(TreeFocus (Node x color left _) zipper) = minimum' $ goLeft focus

--assumes that value that should be successored is focused
successor focus@(TreeFocus (Node _ __ ___ Empty) ____) = 
  let checkUp focus@(TreeFocus currentBranch (TreeZipper [])) = Null
      checkUp focus@(TreeFocus currentBranch (TreeZipper ((_, __, ___, Lft):crumbs))) = goUp focus
      checkUp focus@(TreeFocus currentBranch (TreeZipper ((_, __, ___, Rght):crumbs))) = checkUp $ goUp focus
  in checkUp focus

successor focus@(TreeFocus (Node _ __ ___ right) ____) = minimum' $ goRight focus

--only goes down, assume that is called at the root
append value (TreeFocus Empty zipper) = TreeFocus (leaf value) zipper
append value focus@(TreeFocus (Node x color left right) zipper)
  | value == x = focus --value already exists
  | value < x = append value $ goLeft focus
  | value > x = append value $ goRight focus

--assumes that value that should be deleted is focused
delete focus@(TreeFocus (Node x color Empty Empty) zipper) = TreeFocus Empty zipper
delete focus@(TreeFocus (Node x color node Empty) zipper) = TreeFocus node zipper
delete focus@(TreeFocus (Node x color Empty node) zipper) = TreeFocus node zipper
delete focus@(TreeFocus (Node x color left right) zipper) = updateValue (getValue succ) succDeleted
  where 
    succ = focus -: successor
    succDeleted = succ -: delete -: goTop -: find x

inorder focus@(TreeFocus Empty zipper) = []
inorder focus@(TreeFocus (Node x color left right) zipper) = (inorder $ goLeft focus) ++ [x] ++ (inorder $ goRight focus)

sampleTree1 = (Node 2.0 Red (leaf 1.0) (leaf 3.0))
sampleTree2 = (Node 6.0 Red (leaf 5.0) (leaf 7.0))
sampleContext1 = newFocus sampleTree1

sampleTree3 = (Node 4.0 Red sampleTree1 sampleTree2)
sampleContext3 = newFocus sampleTree3
