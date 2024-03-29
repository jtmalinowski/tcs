import Debug.Trace;
x -: f = f x

data Color = Red | Black deriving(Eq,Show)
data Tree x = 
    Node x Color (Tree x) (Tree x)
  | Empty
  deriving(Show)
leaf x = Node x Red Empty Empty
newTree x = Node x Black Empty Empty

data Direction = Lft | Rght deriving(Eq,Show)
--TreeZipper 1. other not chosen tree 2. direction of the chosen tree
data TreeZipper a = TreeZipper [(a, Color, Tree a, Direction)] deriving(Show)
newZipper = TreeZipper []

data TreeFocus a = TreeFocus (Tree a) (TreeZipper a) | Null deriving(Show)
newFocus tree = TreeFocus tree newZipper

updateValue value (TreeFocus (Node _ color left right) zipper) = (TreeFocus (Node value color left right) zipper)
updateColor color (TreeFocus (Node x _ left right) zipper) = (TreeFocus (Node x color left right) zipper)
getValue (TreeFocus (Node x color left right) zipper) = x
isRed (TreeFocus Empty _) = False
isRed (TreeFocus (Node _ color _ _) _) = color == Red

goLeft  (TreeFocus (Node x color left right) (TreeZipper crumbs)) = TreeFocus left  (TreeZipper $ (x, color, right, Lft):crumbs)
goRight (TreeFocus (Node x color left right) (TreeZipper crumbs)) = TreeFocus right (TreeZipper $ (x, color, left, Rght):crumbs)

goUp (TreeFocus currentBranch (TreeZipper ((value, color, otherBranch, direction):crumbs) ))
  | direction == Lft =  TreeFocus (Node value color currentBranch otherBranch) (TreeZipper crumbs)
  | direction == Rght = TreeFocus (Node value color otherBranch currentBranch) (TreeZipper crumbs)
goTop focus@(TreeFocus tree (TreeZipper crumbs))
  | length crumbs == 0 = focus
  | otherwise = goTop $ goUp focus

isTop focus@(TreeFocus _ (TreeZipper crumbs)) = length crumbs == 0
uncle focus@(TreeFocus _ (TreeZipper (____:(_, __, ___, Lft):crumbs)))   = focus -: goUp -: goUp -: goRight
uncle focus@(TreeFocus _ (TreeZipper (____:(_, __, ___, Rght):crumbs)))  = focus -: goUp -: goUp -: goLeft

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
append value (TreeFocus Empty zipper) = TreeFocus (leaf value) zipper -: appendFixup
append value focus@(TreeFocus (Node x color left right) zipper)
  | value == x = focus --value already exists
  | value < x = append value $ goLeft focus
  | value > x = append value $ goRight focus

appendFixup focus@(TreeFocus _ (TreeZipper ((_, _, _, direction):(_, _, _, pDirection):_)))  
  | parentIsBlack focus = focus
  | uncleIsRed focus = focus -: goUp -: goUp -: updateColor Red -: makeChildrenBlack -: appendFixup
  | direction == Rght && pDirection == Rght = focus -: goUp -: goUp -: leftRotate -: updateColor Black -: colorLeftChild Red -: goRight -: appendFixup
  | direction == Lft && pDirection == Rght = focus -: goUp -: rightRotate -: goRight -: appendFixup
  | direction == Lft && pDirection == Lft = focus -: goUp -: goUp -: rightRotate -: updateColor Black -: colorRightChild Black -: goLeft -: appendFixup
  | direction == Rght && pDirection == Lft = focus -: goUp -: leftRotate -: goLeft -: appendFixup
  where 
    uncleIsRed focus'@(TreeFocus _ (TreeZipper (_:_:_))) = focus' -: uncle -: isRed
    uncleIsRed _ = False
    parentIsBlack focus' = not $ focus' -: goUp -: isRed
    makeChildrenBlack focus' = focus' -: goLeft -: updateColor Black -: goUp -: goRight -: updateColor Black -: goUp
    colorLeftChild color focus' = focus' -: goLeft -: updateColor color -: goUp
    colorRightChild color focus' = focus' -: goRight -: updateColor color -: goUp

appendFixup focus@(TreeFocus _ (TreeZipper [])) = updateColor Black focus 
appendFixup focus@(TreeFocus _ (TreeZipper ((_, _, _, direction):[]))) = focus
appendFixup focus@(TreeFocus (Node _ Black _ _) _) = focus

appendRet value focus = focus -: append value -: goTop

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
depth (TreeFocus Empty _) = 0
depth focus = (max (depth $ goLeft focus) (depth $ goRight focus)) + 1

fromList (x:xs) = 
  let
    startFocus = newTree x -: newFocus 
    appender focus val = appendRet val focus
  in foldl appender startFocus xs
-- RB Tree operations

leftRotate (TreeFocus (Node x color left (Node rightX rightColor rightLeft rightRight)) zipper) =
  TreeFocus (Node rightX rightColor (Node x color left rightLeft) rightRight) zipper

rightRotate (TreeFocus (Node x color (Node leftX leftColor leftLeft leftRight) right) zipper) =
  TreeFocus (Node leftX leftColor leftLeft (Node x color leftRight right)) zipper
