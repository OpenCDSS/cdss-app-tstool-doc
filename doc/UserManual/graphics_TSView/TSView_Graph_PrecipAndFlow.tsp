[Product]

#OutputFile = "C:\temp\sam2.jpg"
#CurrentDateTime = "1950-01"
#PeriodStart = "1914-01"
#PeriodStart = "2002-12"

[SubProduct 1]

MainTitleString = "Precipitation"
GraphType=Bar

[Data 1.1]

TSID=7337.NOAA.Precip.Month~HydroBase

[SubProduct 2]

MainTitleString = "Streamflow"
#LeftYAxisMin = -10000
LegendFormat = "%D, %L"

[Data 2.1]

TSID=08235350.USGS.Streamflow.Month~HydroBase
SymbolStyle=Circle-Filled
SymbolSize=6

[Data 2.2]

TSID=08236000.DWR.Streamflow.Month~HydroBase
SymbolStyle=Circle-Filled
SymbolSize=6
