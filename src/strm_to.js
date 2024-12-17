// Import data
var dataset = ee.Image('CSP/ERGo/1_0/Global/SRTM_topoDiversity');

// Clip to boundary of TO
var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry()

// Visualize data                
var srtmTopographicDiversity = dataset.select('constant').clip(geometry);
var srtmTopographicDiversityVis = {
  min: 0,
  max: 1,
};
Map.setCenter(-79.3832, 43.6532, 9);
Map.addLayer(
    srtmTopographicDiversity, srtmTopographicDiversityVis,
    'SRTM Topographic Diversity');

// Export to Google Drive 
var crs_utm_17N = 'PROJCS["NAD83 / UTM zone 17N",GEOGCS["NAD83",DATUM["North_American_Datum_1983",SPHEROID["GRS 1980",6378137,298.257222101],TOWGS84[0,0,0,0,0,0,0]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4269"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-81],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","26917"]]';

Export.image.toDrive({
  image: srtmTopographicDiversity,
  crs: crs_utm_17N, 
  description: 'strm_to_3m_res',
  maxPixels: 145309360,
  scale: 3
})
