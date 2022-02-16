This folder includes all the IMAGE output we used in this study

IMAGE variable names includes the variable (Dimension) name used for IMAGE


Scenarios:
SSP2_Baseline folder includes all the IMAGE output under "SSP2-Baseline" scenario.  
SSP2_RCP19_no_DACCS folder includes all the IMAGE output under "SSP2-RCP1.9 w/o DACCS" scenario. 
SSP2_RCP19_with_DACCS folder includes all the IMAGE output under "SSP2-RCP1.9 w/ DACCS" scenario. 

Here are the explanations of the data names in each folder:

"ElecProdSpec.out" -- Electricity production per technology (Dimension: Region * Technology, Unit: GJ-electric/yr)
"CO2Spec2.out" -- CO2 emission data (Dimension: Region * Sector * Fuel_type, Unit: Pg C/yr)
"ElecNetDemand.out" -- Electricity demand by sector (Dimension: Region * Sector, Unit: GJ-electric/yr)
"CapInvDAC.out" -- Annual new installed capacity of DACCS (Dimension: Region, Unit: Gt CO2/yr)
"CapDAC.out" -- Annual operational capacity of DACCS (Dimension: Region, Unit: Gt CO2/yr)
"tpes.out" -- Annual primary energy consumption (Dimension: Region * Fuel_type, Unit: PJ primary energy/yr)
"CarbonCapturedSpec2.out" -- Annual CO2 sequestration (Dimension: Region * Sector * Fuel_type, Unit: Pg C/yr)
"ENEFCH4.out" -- Methane emission data (Dimension: Region * Sector * Fuel_type, Unit: Tg CH4/GJ)
"ENEFCO.out" -- Carbon monoxide emission data (Dimension: Region * Sector * Fuel_type, Unit: Tg CO/GJ)
"ENEFN2O.out" -- Dinitrogen monoxide emission data (Dimension: Region * Sector * Fuel_type, Unit: Tg N2O/GJ)
"ENEFNOx.out" -- Nitrogen oxides emission data (Dimension: Region * Sector * Fuel_type, Unit: Tg NO2/GJ)
"ENEFSO2.out" -- Sulfur dioxide emission data (Dimension: Region * Sector * Fuel_type, Unit: Tg SO2/GJ)
"ENEFBC.out" -- Black carbon emission data (Dimension: Region * Sector * Fuel_type, Unit: kg black carbon/GJ)
"nuclfueleff.out" -- Efficiency of nuclear power plants in (Dimension: Region, Unit: GJ electricity/kg uranium)
"ElecEffAvg.out" -- Efficiency of fossil fuel power plant (Dimension: Region * Technology, Unit: GJ-electric/GJ-fuel)




