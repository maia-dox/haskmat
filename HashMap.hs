module HashMap where

data HashMap k v = HashMap [(k,v)]
                   deriving(Show, Eq)

hash_search :: (Eq k) => k -> (HashMap k v) -> v
hash_search key (HashMap ((k,v):ks)) 
        | key == k = v
        | otherwise = hash_find key (HashMap ks)

hash_add :: (Eq k) => k -> (HashMap k Int) -> (HashMap k Int)
hash_add y (HashMap keys) = HashMap (h_search y keys)
        where
        h_search :: (Eq k) => k -> [(k,Int)] -> [(k,Int)]
        h_search key ((k,v):ks)
                | key == k = (k, (v+1)): ks
                | otherwise = (k,v) : (h_search key ks)
        h_search key _ = [(key, 1)]


