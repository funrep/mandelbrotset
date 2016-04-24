height = 50
width = 80
maxiter = 30

isInMandelBrot :: Int -> (Double, Double) -> (Double, Double) -> Bool
isInMandelBrot 30 _ _ = True
isInMandelBrot n (cRe, cIm) (zRe, zIm) =
    let (zRe2, zIm2) = (zRe**2, zIm**2)
    in if zRe2 + zIm2 >= 4
        then False
        else isInMandelBrot (n+1)
                            (cRe, cIm)
                            (zRe2 - zIm2 + cRe, 2*zRe*zIm + cIm)

col :: Double -> Double -> [Bool]
col 80 _ = []
col x y = isInMandelBrot 0 (scaleX x, scaleY y) (0, 0) : col (x+1) y

row :: Double -> [[Bool]]
row 50 = []
row y = col 0 y : row (y + 1)

scaleX :: Double -> Double
scaleX x = (x - width/2.0) * (4.0/width)

scaleY :: Double -> Double
scaleY y = (y - height/2.0) * (4.0/width)

render :: [[Bool]] -> String
render [] = ""
render (x:xs) = go x ++ "\n" ++ render xs
    where
        go [] = ""
        go (x:xs) =
            if x
            then "+" ++ go xs
            else " " ++ go xs

main :: IO ()
main = putStrLn . render $ row 0
