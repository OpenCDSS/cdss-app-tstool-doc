[Product]

MainTitleString = ""
ProductType = "Graph"
TotalHeight = "400"
TotalWidth = "600"

[SubProduct 1]

GraphType = "Bar"
LeftYAxisTitleString = "IN"
MainTitleString = "Precipitation"
RightYAxisGraphType = "None"
RightYAxisIgnoreUnits = "false"
BarPosition = "LeftOfDate"
BarOverlap = "False"
LayoutYPercent = ""

[Data 1.1]

FlaggedDataSymbolStyle = ""
LineStyle = "None"
TSID = "USC00050125.NOAA.Precip.Month~HydroBase"

[SubProduct 2]

GraphType = "Line"
LeftYAxisLabelPrecision = "0"
MainTitleString = "Streamflow"
LayoutYPercent = ""

[Data 2.1]

TSID = "08236000.DWR.Streamflow.Month~HydroBase"
Color = "Blue"

[Data 2.2]

Color = "Red"
SymbolSize = "5"
SymbolStyle = "Circle-Filled"
TSID = "08236500.DWR.Streamflow.Month~HydroBase"
