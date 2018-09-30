-- For a circle we need a point and a radius. For a rectangle, we need two points.
data Shape a = Circle (a, a) a | Rectangle (a, a) (a, a) deriving (Show)  

shapeArea :: (Fractional a) => Shape a -> a
shapeArea (Circle (x, y) radius) = 0.5 * 3.14 * radius ^ 2
shapeArea (Rectangle (x1, y1) (x2, y2)) = (x1-x2) * (y1-y2)