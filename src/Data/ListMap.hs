module Data.ListMap
    ( insert
    , delete
    , atMay
    ) where

insert :: Int -> a -> [a] -> [a]
insert 0 x ys = x : ys
insert n x (y:ys) = y : insert (pred n) x ys
insert _ x [] = [x]

delete :: Int -> [a] -> [a]
delete _ [] = []
delete 0 (_:xs) = xs
delete n (x:xs) = x : delete (pred n) xs

atMay :: Int -> [a] -> Maybe a
atMay _ [] = Nothing
atMay 0 (x:_) = Just x
atMay n (_:xs) = atMay (pred n) xs
