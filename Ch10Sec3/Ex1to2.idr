module Ex1to2

-- import DataStore
-- import Shape_abs

import Ch10Sec3.DataStore
import Ch10Sec3.Shape_abs

getValues : DataStore (SString .+. val_schema) ->
                List (SchemaType val_schema)
getValues x with (storeView x)
  getValues x | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) = 
    value :: getValues store | rec

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

area : Shape -> Double
area s with (shapeView s)
  area (triangle b h) | STriangle = 0.5 * b * h
  area (rectangle b h) | SRectangle = b * h
  area (circle r) | SCircle = pi * r * r
  