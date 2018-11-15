palins :: [Char] -> Int
palins str@((a::Char):b)
      | null b = 1
      | head str == head $ reverse str = normal_palins //TODO: rest
      | otherwise = normal_palins
      where
        normal_palins = (palins $ tail str) + (palins $ init str) - (palins $ init $ tail str)
