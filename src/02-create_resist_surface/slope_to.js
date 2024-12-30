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
Export.image.toDrive({
  image: slope_to,
  description: 'slope_to_3m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:26917', 
  maxPixels: 209242992,
  scale: 2.5}  
  );
