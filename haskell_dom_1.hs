type Rect = (Int,Int,Int,Int)
type Grade = (Int,Rect)

x1   (a,_,_,_) = a
y1   (_,a,_,_) = a
x2   (_,_,a,_) = a
y2   (_,_,_,a) = a
gr   (a,_)     = a
rect (_,a)     = a

away :: Rect -> Rect -> Bool
away v1 v2 = if (x2 v1) < (x1 v2) ||
                (y2 v1) < (y1 v2) ||
				(x1 v1) > (x2 v2) ||
                (y1 v1) > (y2 v2)
				 then True else False

inRect :: Rect -> Rect -> Bool
inRect v1 v2 = if (not (away v1 v2)) then True else False

crossed :: [Rect] -> Rect -> Int -> Grade
crossed l x cnt
	| null l                                 = (cnt, x)
	| (inRect x (head l)) && ((head l) /= x) = crossed (tail l) x (cnt+1)
	| otherwise                              = crossed (tail l) x cnt

maxX :: [Grade] -> Int -> Int
maxX grades max
	| null grades              = max
	| (gr (head grades)) > max = maxX (tail grades) (gr (head grades))
	| otherwise                = maxX (tail grades) max

plot :: Rect -> Int
plot v = (((y2 v) - (y1 v))*((x2 v) - (x1 v)))
	
maxPlot :: [Rect] -> Rect -> Int -> Rect
maxPlot rects maxR maxP 
	| null rects        = maxR
	| (plot cur) > maxP = maxPlot (tail rects) cur (plot cur)
	| otherwise         = maxPlot (tail rects) maxR maxP
	where cur = (head rects)
		  	
grades l = [ crossed l x 0 | x <- l ]

mosts l = [ (rect x) | x <- l, (gr x) == (maxX l 0)]
	
mostPopular list = maxPlot (mosts (grades list)) (0,0,0,0) 0

