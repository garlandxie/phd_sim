// Import digital surface model 
var dataset = ee.ImageCollection('JAXA/ALOS/AW3D30/V3_2')
var elevation = dataset.select('DSM');

// Reproject an image mosaic using a projection from one of the image tiles,
// rather than using the default projection returned by .mosaic().
var proj = elevation.first().select(0).projection();
var slopeReprojected = ee.Terrain.slope(elevation.mosaic()
                                 .setDefaultProjection(proj));

// Clip to boundary of Toronto
var geometry = 
    /* color: #00ffff */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-79.71665989314526, 43.810689250599644],
          [-79.71665989314526, 43.56839350093264],
          [-79.1694034234187, 43.56839350093264],
          [-79.1694034234187, 43.810689250599644]]], 
          null, false);

var slope_to = slopeReprojected.clip(geometry);

// Visualize
Map.setCenter(-79.72377617187499, 43.864467866301716, 11);
Map.addLayer(slope_to, {min: 0, max: 45}, 'Slope');

// Export to Google Drive
Export.image.toDrive({
  image: slope_to,
  description: 'slope_to_30m',
  scale: 30}  
  )
