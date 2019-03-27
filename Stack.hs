module Stack where


data Stack a = Stack [a]
        deriving (Show, Eq)


stack_push :: Stack a -> a -> Stack a
stack_push (Stack xs) x = Stack (x:xs)


stack_peek :: Stack a -> a
stack_peek (Stack (x:xs)) = x


stack_rm :: Stack a -> Stack a
stack_rm (Stack (x:xs)) = Stack xs
stack_rm (Stack []) = Stack []


stack_isEmpty :: Stack a -> Bool
stack_isEmpty (Stack []) = True
stack_isEmpty _ = False

