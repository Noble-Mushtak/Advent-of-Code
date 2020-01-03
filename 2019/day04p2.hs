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

firstTwoAreEqual :: [Int] -> Bool
firstTwoAreEqual (a:b:c:_) = (a == b) && (b /= c)
firstTwoAreEqual _ = False

twoAdjacentAreEqual :: [Int] -> Bool
twoAdjacentAreEqual (a:rst@(b:c:d:_))
  | (a /= b) && (b == c) && (c /= d) = True
  | otherwise                        = twoAdjacentAreEqual rst
twoAdjacentAreEqual (a:rst@(b:c:_))
  | (a /= b) && (b == c) = True
  | otherwise            = twoAdjacentAreEqual rst
twoAdjacentAreEqual _ = False

valid :: [Int] -> Bool
valid = (&&) <$> nonDecreasing <*> ((||) <$> twoAdjacentAreEqual <*> firstTwoAreEqual)

main :: IO ()
main = print $ sum $ map (fromEnum . valid . digits) [rangeMin..rangeMax]