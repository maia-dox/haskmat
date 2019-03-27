module Ackermann where

-- m = 0: n + 1
-- (m > 0 && m n == 0): A(M-1, 1)
-- (m > 0 && n > 0): A(m-1, A(m,n-1))

ackermann :: Integer -> Integer -> Integer
ackermann m n 
	| m == 0 = n + 1
	| (m > 0) && (n == 0) = ackermann (m-1) 1
	| (m > 0) && (n > 0) = ackermann (m-1) (ackermann m (n-1))

