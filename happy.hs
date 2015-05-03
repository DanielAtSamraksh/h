

# inspired by http://www.smashcompany.com/technology/embarrassing-code-i-wrote-under-stress-at-a-job-interview
nextSeq:: Int-> Int-> Int
nextSeq 0 acc = acc
nextSeq x acc = nextSeq (x `div` 10) (acc + m*m)
        where m = x `mod` 10

isHappy:: Int-> [Int]-> Bool
isHappy 1 _ = True
isHappy x lst = notElem x lst && isHappy (nextSeq x 0) (x:lst)
happy:: Int -> Bool
happy x = isHappy x []
happies = filter happy [1..]


