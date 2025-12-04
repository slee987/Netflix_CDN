// ------------------------------
// 1. MODIS NDVI (MOD13Q1)
// ------------------------------
var modisNDVI = ee.ImageCollection("MODIS/006/MOD13Q1")
                  .select("NDVI");

// ------------------------------
// 2. 2011 year avg NDVI
// (MODIS NDVI는 16-day composite)
// ------------------------------
var ndvi_2011 = modisNDVI
    .filterDate("2011-01-01", "2012-01-01")  // full year
    .mean()
    .multiply(0.0001)  // scaling factor
    .rename("NDVI_2011_mean");

// Checking Map
Map.addLayer(ndvi_2011, {min:0, max:1, palette:['white','green']}, 'NDVI 2011');

// ------------------------------
// 3. Call Bounding Boxes
// ------------------------------
var regions = ee.FeatureCollection("projects/ee-my-smlee/assets/px_u_map_43261");

// Box
Map.addLayer(regions.style({color:'red', fillColor:'00000000'}), {}, "Pixel Boxes");
Map.centerObject(regions, 12);


// ------------------------------
// 4. AVG NDVI by box
// ------------------------------
var attachNDVI = function(feature) {
  
  var stats = ndvi_2011.reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: feature.geometry(),
    scale: 250,           // MODIS NDVI = 250m
    maxPixels: 1e13
  });
  
  var ndvi_val = stats.get("NDVI_2011_mean");
  
  return feature.set("NDVI_2011", ndvi_val);
};

var result = regions.map(attachNDVI);

print(result.limit(5));


// ------------------------------
// 5. Export to Drive
// ------------------------------
Export.table.toDrive({
  collection: result,
  description: "NDVI_2011_217_boxes",
  folder: "EE_NDVI_Results",
  fileNamePrefix: "NDVI_2011",
  fileFormat: "CSV"
});
