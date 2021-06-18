module Utils.List (evens, odds)
where 

evens :: [a] -> [a]
evens [] = []
evens [x] = [x]
evens (x:y:ys) = x : evens ys

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = evens xs