# Dam and Violence in Africa

This research investigates how large hydropower dams intensified the inter-ethnic and cross-boundary conflicts among downstream communities through their distributional effect on water and other natural resources. Utilizing a triple DID design that exploits variations from a) proximity to dams, b) pre- or post-dam period, and c) dam-truncated or non-truncated tributaries, this paper demonstrates a significant increase in conflicts along truncated tributaries in the near-dam downstream areas following dam commissioning. 

1_Delineate_River.R: combine dams, river basins, rivers, topology... based on their geo-location. It also delinate the relationship between dams and river segments (upstream or downstream)

2_GridCellData.R: rasterize all sample into 10km*10km grid cells, and match up for each cell based on cell ID.

3_Conflicts.R: conflict data cleaning. Convert conflict data to monthly-cell-dam level panel data. Construct main dependent variables: number of conflicts and conflict dummy.

4_ConflictsMonthlyComb.R: Combine monthly conflict data with dam- and river-related variables obtained from 2_GridCellData.R.

5_Landcover_panel.R: Yearly land cover data cleaning, rasterize and resample into cell-level data.

6_Landcover_mergewith_Conflicts.R: Merge land cover data with conflict panel data obtained from 4_ConflictsMonthlyComb.R.

7_Rainfall_panel.R: Collect monthly rainfall data, rasterize and resample into cell-level data.

8_Groundwater_panel.R: Collect monthly groundwater storage data (proxied by GRACE Land Water Equivalent Thickness),rasterize and resample into cell-level data.

9_SoilMoisture_panel.R: Collect monthly soil moisture data, rasterize and resample into cell-level data.

10_Drought_panel.R: Collect monthly drought severity data, rasterize and resample into cell-level data.

11_WaterDef_panel.R: Collect monthly water deficit data, rasterize and resample into cell-level data.

12_GWS_panel.R: Collect monthly groundwater storage data, rasterize and resample into cell-level data.

13_Construct_Controls.R: Construct other control variables, rasterize and resample into cell-level data.

14_Construct_Controls.R: Merge control variables and water-ralated variable from rainfall to groundwater storage with conflict-dam sample (obtained from 6_Landcover_mergewith_Conflicts.R).

15_DHS_Processing.R: Data cleaning for the Demographic and Health Survey data (Phases II-IIIV). Merge the DHS sample (household level) with conflict-dam sample (obtained from 6_Landcover_mergewith_Conflicts.R).

16_Regression.R: Regression and store results

17_Graphics.R: Graphics including maps.
