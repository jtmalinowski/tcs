data Tree x = 
    Node x (Tree x) (Tree x)
  | Empty
  deriving(Show)
leaf x = Node x Empty Empty


data Direction = Lft | Rght deriving(Eq,Show)
--TreeZipper 1. other not chosen tree 2. direction of the chosen tree
data TreeZipper a = TreeZipper [(a, Tree a, Direction)] deriving(Show)
newZipper = TreeZipper []

data TreeFocus a = TreeFocus (Tree a) (TreeZipper a) deriving(Show)
newFocus tree = TreeFocus tree newZipper
goLeft  (TreeFocus (Node x left right) (TreeZipper crumbs)) = TreeFocus left  (TreeZipper $ (x, right, Lft):crumbs)
goRight (TreeFocus (Node x left right) (TreeZipper crumbs)) = TreeFocus right (TreeZipper $ (x, left, Rght):crumbs)

goUp (TreeFocus currentBranch (TreeZipper ((value, otherBranch, direction):crumbs) ))
  | direction == Lft =  TreeFocus (Node value currentBranch otherBranch) (TreeZipper crumbs)
  | direction == Rght = TreeFocus (Node value otherBranch currentBranch) (TreeZipper crumbs)
goTop focus@(TreeFocus tree (TreeZipper crumbs))
  | length crumbs == 0 = focus
  | otherwise = goTop $ goUp focus

--only goes down, assume that is called at the root
append value (TreeFocus Empty zipper) = TreeFocus (leaf value) zipper
append value focus@(TreeFocus (Node x left right) zipper)
  | value == x = focus --value already exists
  | value < x = append value $ goLeft focus
  | value > x = append value $ goRight focus

sampleTree1 = (Node 2.0 (leaf 1.0) (leaf 3.0))
sampleTree2 = (Node 6.0 (leaf 5.0) (leaf 7.0))
sampleContext1 = newFocus sampleTree1

sampleTree3 = (Node 4.0 sampleTree1 sampleTree2)
sampleContext3 = newFocus sampleTree3

x -: f = f x
