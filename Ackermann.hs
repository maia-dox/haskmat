module Ackermann where

-- m = 0: n + 1
-- (m > 0 && m n == 0): A(M-1, 1)
-- (m > 0 && n > 0): A(m-1, A(m,n-1))

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 = (ackermann (m-1) 1)
ackermann m n = ackermann (m-1) (ackermann m (n-1))




super_ack :: Integer -> Integer -> Integer
super_ack 0 n = n + 1
super_ack m 0 = super_ack (m-1) 1
super_ack m n = super_ack (m-1) (super_ack m (super_ack m (n-1)) )


