// Import dataset 
var soil_water = ee.Image('OpenLandMap/SOL/SOL_WATERCONTENT-33KPA_USDA-4B1C_M/v01');
var soil_clay = ee.Image("OpenLandMap/SOL/SOL_CLAY-WFRACTION_USDA-3A1A1A_M/v02")
var soil_ph = ee.Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02")
var soil_sand = ee.Image("OpenLandMap/SOL/SOL_SAND-WFRACTION_USDA-3A1A1A_M/v02")

// Clip boundary to TO
var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry();
                 
var sw_to = soil_water.select('b30').clip(geometry)
var sc_to = soil_clay.select('b30').clip(geometry)
var ph_to = soil_ph.select('b30').clip(geometry)
var sand_to = soil_sand.select('b30').clip(geometry)

// Visualize data

// Soil Water
var soil_water_viz = {
  bands: ['b30'],
  min: 0.0,
  max: 52.9740182135385,
  palette: [
    'd29642','eec764','b4ee87','32eeeb','0c78ee','2601b7',
    '083371',
  ]
};

// Soil Bulk Density
var soil_bd_viz = {
  bands: ['b30'],
  min: 5.0,
  max: 185.0,
  palette: ['5e3c99', 'b2abd2', 'f7e0b2', 'fdb863', 'e63b01']
};

// Soil Clay
var soil_clay_viz = {
  bands: ['b30'],
  min: 2,
  max: 100,
  palette: [
    'ffff00', 'f8f806', 'f1f10c', 'ebeb13', 'e4e419', 'dddd20',
    'd7d726', 'd0d02d', 'caca33', 'c3c33a', 'bcbc41', 'b6b647',
    'b0b04e', 'a9a954', 'a3a35a', '9c9c61', '959568', '8f8f6e',
    '898975', '82827b', '7b7b82', '757589', '6e6e8f', '686895',
    '61619c', '5a5aa3', '5454a9', '4d4db0', '4747b6', '4141bc',
    '3a3ac3', '3333ca', '2d2dd0', '2626d7', '2020dd', '1919e4',
    '1212eb', '0c0cf1', '0606f8', '0000ff',
  ]
};

// Soil pH
var soil_ph_viz = {
  bands: ['b30'],
  min: 0.0,
  max: 120.0,
  palette: [
    'ffffa0','f7fcb9','d9f0a3','addd8e','78c679','41ab5d',
    '238443','005b29','004b29','012b13','00120b',
  ]
};

// Soil sand
var soil_sand_viz = {
  bands: ['b30'],
  min: 1.0,
  max: 100.0,
  palette: [
    'ffff00', 'f8f806', 'f1f10c', 'ebeb13', 'e4e419', 'dddd20',
    'd7d726', 'd0d02d', 'caca33', 'c3c33a', 'bcbc41', 'b6b647',
    'b0b04e', 'a9a954', 'a3a35a', '9c9c61', '959568', '8f8f6e',
    '898975', '82827b', '7b7b82', '757589', '6e6e8f', '686895',
    '61619c', '5a5aa3', '5454a9', '4d4db0', '4747b6', '4141bc',
    '3a3ac3', '3333ca', '2d2dd0', '2626d7', '2020dd', '1919e4',
    '1212eb', '0c0cf1', '0606f8', '0000ff',
  ]
};

Map.centerObject(sw_to);
Map.addLayer(sw_to, soil_water_viz, 'Soil water content at 33kPa (field capacity)');
Map.addLayer(sc_to, soil_clay_viz, 'Clay content in % (kg / kg)');
Map.addLayer(ph_to, soil_ph_viz, 'Soil pH x 10 in H2O');
Map.addLayer(sand_to, soil_sand_viz, 'Sand content in % (kg / kg)');

// Export to Google Drive 

// Soil Water
Export.image.toDrive({
  image: sw_to,
  crs: 'EPSG:26917',
  description: 'soil_water_to_20m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  maxPixels: 209242992,
  scale: 20
});


// Soil Clay
Export.image.toDrive({
  image: sc_to,
  crs: 'EPSG:26917',
  description: 'soil_clay_to_20m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  maxPixels: 209242992,
  scale: 20
});

// Soil pH
Export.image.toDrive({
  image: ph_to,
  crs: 'EPSG:26917',
  description: 'soil_pH_to_20m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  maxPixels: 209242992,
  scale: 20
});

// Soil Sand
Export.image.toDrive({
  image: sand_to,
  crs: 'EPSG:26917',
  description: 'soil_sand_to_20m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  maxPixels: 209242992,
  scale: 20
});
