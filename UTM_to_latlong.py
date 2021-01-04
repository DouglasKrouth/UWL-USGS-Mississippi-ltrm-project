# utm package is for converting utm to lat/long
import utm
import pandas as pd
from time import sleep
from tqdm import tqdm
# Show all columns, rows on print
pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

# Customize paths to water and veg files as needed
# "ltrm_vegsrs_data.csv"
# "ltrm_water_data.csv"
water_path = r"C:\Users\Doug\Documents\Classes\TDA\ltrm_water_data.csv"
veg_path = r"C:\Users\Doug\Documents\Classes\TDA\ltrm_vegsrs_data.csv"

# Create DataFrames containing ltrm_water and ltrm_vegsrs data
df_water = pd.read_csv(water_path, low_memory=False)
df_veg = pd.read_csv(veg_path, low_memory=False)

# WATER

# Create a temp-DataFrame only containing 'SHEETBAR', 'LATITUDE', 'LONGITUDE' columns; these will be filled during iteration across DataFrames
# SHEETBAR : Merge key to join lat/long values to their matching sample
# LATITUDE, LONGITUDE : Calculated Latitude/Longitude pair that is derived from UTM EASTING, UTM NORTHING codes
temp_water = pd.DataFrame(columns=['SHEETBAR', 'LATITUDE', 'LONGITUDE'])

# dictionary with counters to track how many rows have missing values
missing_row_count = {'water': 0, 'vegsrs': 0}

for index, row in tqdm(df_water.iterrows()):
    # Base case to check if row is missing EASTING, NORTHING values; if so, print row and iterate missing_row_count, then skip the row
    if pd.isnull(row['EASTING']) or pd.isnull(row['NORTHING']):
        print(row)
        missing_row_count['water'] += 1
        continue
    # UTM ZONE is in Northern Hemisphere, so northern = True
    lat_long_temp = utm.to_latlon(row['EASTING'], row['NORTHING'], row['UTMZONE'], northern=True)
    # Pull values
    latitude_temp = lat_long_temp[0]
    longitude_temp = lat_long_temp[1]
    # Create a dictionary that can be appended to temp with lat, long values
    d = {'SHEETBAR': row['SHEETBAR'], 'LATITUDE': latitude_temp, 'LONGITUDE': longitude_temp}
    # DEBUG : Print the newly created row value
    # print(d, "INDEX:", index)
    temp_water = temp_water.append(d, ignore_index=True)

    
# VEGSRS
# Create a temp-DataFrame only containing 'BARCODE', 'LATITUDE', 'LONGITUDE' columns; these will be filled during iteration across DataFrames
# BARCODE : Merge key to join lat/long values to their matching sample
# LATITUDE, LONGITUDE : Calculated Latitude/Longitude pair that is derived from EAST1, NORTH1 codes
temp_veg = pd.DataFrame(columns=['BARCODE', 'LATITUDE', 'LONGITUDE'])

for index, row in tqdm(df_veg.iterrows()):
    # Base case to check if row is missing EASTING, NORTHING values; if so, print row and iterate missing_row_count, then skip the row
    if pd.isnull(row['EAST1']) or pd.isnull(row['NORTH1']):
        print(row)
        missing_row_count['vegsrs'] += 1
        continue
    # UTM ZONE is in Northern Hemisphere, so northern = True
    lat_long_temp = utm.to_latlon(row['EAST1'], row['NORTH1'], row['ZONE'], northern=True)
    # Pull values
    latitude_temp = lat_long_temp[0]
    longitude_temp = lat_long_temp[1]
    # Create a dictionary that can be appended to temp with lat, long values
    d = {'BARCODE': row['BARCODE'], 'LATITUDE': latitude_temp, 'LONGITUDE': longitude_temp}
    # DEBUG : Print the newly created row value
    # print(d, "INDEX:", index)
    temp_veg = temp_veg.append(d, ignore_index=True)

# Merge the latitude, longitude values to their respective DataFrames
df_water_output = df_water.merge(temp_water, left_on='SHEETBAR', right_on='SHEETBAR')
df_veg_output = df_veg.merge(temp_veg, left_on='BARCODE', right_on='BARCODE')

# Paths to export data as .csv
water_output_path = r'C:\Users\Doug\Documents\Classes\TDA\ltrm_water_data_lat_long.csv'
veg_output_path = r'C:\Users\Doug\Documents\Classes\TDA\ltrm_vegsrs_data_lat_long.csv'

# Output the data with lat, long to .csv files
df_water_output.to_csv(water_output_path)
df_veg_output.to_csv(veg_output_path)