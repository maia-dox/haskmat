module LinkedList where


data Node a = Node a
        deriving(Show, Eq)

data LinkedList n v = SingleLink (Node n) v | DoubleLinkList (Node n) (Node n) v
        deriving(Show, Eq)


-- example: test = SingleLink (Node (SingleLink (Node 4) 3)) 5

