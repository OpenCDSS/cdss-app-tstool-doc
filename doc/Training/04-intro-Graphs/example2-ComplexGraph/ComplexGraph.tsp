[Product]

MainTitleString = "Kersey Data"
ProductType = "Graph"
TotalHeight = "400"
TotalWidth = "600"

[SubProduct 1]

GraphType = "Line"
LeftYAxisIgnoreUnits = "false"
MainTitleString = "Temperature"
LayoutYPercent = ""

[Data 1.1]

TSID = "3546.NOAA.TempMean.Month"

[Data 1.2]

TSID = "3553.NOAA.TempMean.Month"

[SubProduct 2]

GraphType = "Bar"
MainTitleString = "Precipitation"
BarPosition = "LeftOfDate"
LayoutYPercent = ""

[Data 2.1]

LineStyle = "Solid"
TSID = "3546.NOAA.Precip.Month"

[Data 2.2]

TSID = "3553.NOAA.Precip.Month"

[SubProduct 3]

GraphType = "Line"
MainTitleString = "Streamflow"
LayoutYPercent = ""

[Data 3.1]

TSID = "06754000.DWR.Streamflow.Month"
