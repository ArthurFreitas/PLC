type Ponto = (Float, Float)
type Reta = (Ponto,Ponto)

isVertical :: Reta -> Bool
isVertical ((x1,y1),(x2,y2)) = x1 == x2