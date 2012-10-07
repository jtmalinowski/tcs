data RBTree x = 
    Node x (RBTree x) (RBTree x)
  | Empty
  deriving(Show)

tree x = Node x Empty Empty

append value Empty = tree value
append value tree@(Node x left right)
  | value < x = Node x (append value left) right
  | value > x = Node x left (append value right)
  | value == x = tree

x -: f = f x
