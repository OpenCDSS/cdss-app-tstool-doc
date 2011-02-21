[Product]

ProductType = "Graph"
TotalWidth = "600"
TotalHeight = "400"
MainTitleString = "Comparison of Running Average for Diversion Data"

[SubProduct 1]

GraphType = "Line"
LeftYAxisMax = "14439.8798828125"

[Data 1.1]

TSID = "0100501.DWR.DivTotal.Month~HydroBase"
Color = "Green"

[Data 1.2]

TSID = "0100501.DWR.DivTotal-Running-Mean.Month~HydroBase"
TSAlias = "Centered"
Color = "Red"

[Data 1.3]

TSID = "0100501.DWR.DivTotal-Running-5Mean.Month~HydroBase"
TSAlias = "NYear"
Color = "Blue"
