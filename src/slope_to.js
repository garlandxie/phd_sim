// Import digital surface model 
var dataset = ee.ImageCollection('JAXA/ALOS/AW3D30/V3_2');
var elevation = dataset.select('DSM');

// Reproject an image mosaic using a projection from one of the image tiles,
// rather than using the default projection returned by .mosaic().
var proj = elevation.first().select(0).projection();
var slopeReprojected = ee.Terrain.slope(elevation.mosaic()
                                 .setDefaultProjection(proj));

// Clip to boundary of Toronto
var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry();
var slope_to = slopeReprojected.clip(geometry);

// Visualize
Map.setCenter(-79.72377617187499, 43.864467866301716, 11);
Map.addLayer(slope_to, {min: 0, max: 45}, 'Slope');

// Export to Google Drive
var crs_utm_17N = 'PROJCS["NAD83 / UTM zone 17N",GEOGCS["NAD83",DATUM["North_American_Datum_1983",SPHEROID["GRS 1980",6378137,298.257222101],TOWGS84[0,0,0,0,0,0,0]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4269"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-81],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","26917"]]';

Export.image.toDrive({
  image: slope_to,
  description: 'slope_to_3m_res',
  crs: crs_utm_17N,
  maxPixels: 209242992,
  scale: 2.5}  
  );
