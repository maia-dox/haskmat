module BitConvert where
import Data.Char

-- int -> bit and hex utils


-- unsigned int -> String
uint_to_bits :: Int -> String
uint_to_bits i = uint_convert i 128
        where 
        uint_convert :: Int -> Int -> [Char]
        uint_convert num m
                | m == 0 = []
                | (num == m) || (num > m) = '1' : (uint_convert (num-m) (div m 2))
                | num < m = '0' : (uint_convert num (div m 2))


-- converts signed int to bits
int_to_bits :: Int -> String
int_to_bits i 
        | i >= 0 = '0' : (pos_convert i 64)
        | otherwise = '1' : (carry_add ( flip_bits (pos_convert (-1 * i) 64)))
        where
        pos_convert :: Int -> Int -> [Char]
        pos_convert num m
                | m == 0 = []
                | (num == m) || (num > m) = '1' : (pos_convert (num-m) (div m 2))
                | num < m = '0' : (pos_convert num (div m 2))
        flip_bits :: String -> String
        flip_bits [] = []
        flip_bits (c:cs) 
                | c == '0' = (flip_bits cs) ++ "1" 
                | c == '1' = (flip_bits cs) ++ "0"
        carry_add :: String -> String
        carry_add [] = []
        carry_add (c:cs) 
                | c == '0' = cs ++ "1"
                | c == '1' = (carry_add cs) ++ "0"
                | otherwise = " OVERFLOW"


-- converts bits to unsigned int
bits_to_uint :: String -> Int
bits_to_uint str = bit_convert str 128
        where
        bit_convert :: String -> Int -> Int
        bit_convert [] _ = 0
        bit_convert (x:xs) msb 
                | x == '1' = msb + (bit_convert xs (div msb 2))
                | x == '0' = bit_convert xs (div msb 2)



-- converts hex string to int
hex_to_int :: String -> Int
hex_to_int [] = 0
hex_to_int (c:cs)
        | c == 'A' = (10 * (16^msb)) + (hex_to_int cs)
        | c == 'B' = (11 * (16^msb)) + (hex_to_int cs)
        | c == 'C' = (12 * (16^msb)) + (hex_to_int cs)
        | c == 'D' = (13 * (16^msb)) + (hex_to_int cs)
        | c == 'E' = (14 * (16^msb)) + (hex_to_int cs)
        | c == 'F' = (15 * (16^msb)) + (hex_to_int cs)
        | otherwise = (digitToInt c) + (hex_to_int cs)
        where
        msb = length cs




