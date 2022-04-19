result :: Integer -> Integer
result n = help 1 1 1 n
    where help one two three 1 = 
              one
          help one two three n = 
              help two three (three + two + one) (n - 1)
