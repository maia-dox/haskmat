module BitConvert where


uint_to_bit :: Int -> String
uint_to_bit i = uint_convert i 128
	where 
	uint_convert :: Int -> Int -> [Char]
	uint_convert num m
		| m == 0 = []
		| (num == m) || (num > m) = '1' : (uint_convert (num-m) (div m 2))
		| num < m = '0' : (uint_convert num (div m 2))

int_to_bit :: Int -> String
int_to_bit i 
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







