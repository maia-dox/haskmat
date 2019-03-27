module Queue where


data Queue a = Queue [a]
            deriving(Show, Eq)

queue_add :: Queue a -> a -> Queue a
queue_add (Queue xs) y = Queue (xs ++ [y])

queue_peek :: Queue a -> a
queue_peek (Queue (x:xs)) = x

queue_rm :: Queue a -> Queue a
queue_rm (Queue (x:xs)) = Queue xs

queue_isEmpty :: Queue a -> Bool
queue_isEmpty (Queue []) = True
queue_isEmpty _ = False
