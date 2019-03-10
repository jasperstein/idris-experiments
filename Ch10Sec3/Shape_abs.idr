module Shape_abs

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

public export
data ShapeView: Shape -> Type where
    STriangle: ShapeView (triangle b h)
    SRectangle: ShapeView (rectangle b h)
    SCircle: ShapeView (circle r)

export
shapeView: (shape: Shape) -> (ShapeView shape)
shapeView (Triangle base height) = STriangle {b = base} {h = height}
shapeView (Rectangle base height) = SRectangle {b = base} {h = height}
shapeView (Circle radius) = SCircle {r = radius}
