toDigits :: Int -> [Int]
toDigitsRev :: Int -> [Int]

toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther (x:y:zs) =
  x : (y * 2) : (doubleEveryOther zs)
doubleEveryOther n = n

sumDigits :: [Int] -> Int
sumDigits n =
  sum ((map (`div` 10) n) ++ (map (`mod` 10) n))

validate :: Int -> Bool
validate n =
  (sumDigits (doubleEveryOther (toDigitsRev 2673))) `mod` 10 == 0

removeSpaces :: String -> String
removeSpaces n = filter (/= ' ') n

main =
  let step1 = read (removeSpaces "4111 1111 4555 1142 ") :: Int
      step2 = doubleEveryOther (toDigitsRev step1)
      step3 = sumDigits step2
      result = validate step3
  in  putStrLn ((show step1) ++ "\n" ++ (show step2) ++ "\n" ++ (show step3) ++ "\n" ++ (show result))
