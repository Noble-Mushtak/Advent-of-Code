rangeMin :: Int
rangeMin = 136760
rangeMax :: Int
rangeMax = 595730

digits :: Int -> [Int]
digits = map (read . (:"")) . show

nonDecreasing :: [Int] -> Bool
nonDecreasing (a:rst@(b:_))
  | a > b     = False
  | otherwise = nonDecreasing rst
nonDecreasing _ = True

twoAdjacentAreEqual :: [Int] -> Bool
twoAdjacentAreEqual (a:rst@(b:_))
  | a == b    = True
  | otherwise = twoAdjacentAreEqual rst
twoAdjacentAreEqual _ = False

valid :: [Int] -> Bool
valid = (&&) <$> nonDecreasing <*> twoAdjacentAreEqual

main :: IO ()
main = print $ sum $ map (fromEnum . valid . digits) [rangeMin..rangeMax]