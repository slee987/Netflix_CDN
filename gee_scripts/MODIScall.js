// ======================================================================
// 0. SETTINGS
// ======================================================================

var regions = ee.FeatureCollection("projects/ee-my-smlee/assets/px_u_map");

var START = '2011-10-01';
var END   = '2016-12-31';

var TERRA = 'MODIS/061/MOD11A2';
var AQUA  = 'MODIS/061/MYD11A2';


// // ======================================================================
// // 1. QC + Kelvin RAW 전처리
// // ======================================================================

// var preprocess = function(img){
  
//   var qcDay = img.select('QC_Day');
//   var qcNight = img.select('QC_Night');
  
//   var dayMask = qcDay.lt(2);
//   var nightMask = qcNight.lt(2);
  
//   var day = img.select('LST_Day_1km')
//                 .updateMask(dayMask)
//                 .rename('LST_Day_K');

//   var night = img.select('LST_Night_1km')
//                   .updateMask(nightMask)
//                   .rename('LST_Night_K');

//   return day.addBands(night)
//             .copyProperties(img, ['system:time_start']);
// };


// // ======================================================================
// // 2. MODIS Terra + Aqua 데이터 불러오기
// // ======================================================================

// var terra = ee.ImageCollection(TERRA)
//       .filterDate(START, END)
//       .filterBounds(regions)
//       .map(preprocess);

// var aqua = ee.ImageCollection(AQUA)
//       .filterDate(START, END)
//       .filterBounds(regions)
//       .map(preprocess);

// var allLST = terra.merge(aqua).sort("system:time_start");

// print("Total MODIS Images:", allLST.size());


// // ======================================================================
// // 3. 날짜 리스트를 먼저 뽑아서 client로 가져오기 (안전함!)
// // ======================================================================

// // 각 이미지의 system:time_start 날짜만 모아서 리스트 생성
// var dateList = allLST.aggregate_array('system:time_start');  
// var dateListClient = dateList.getInfo();   // ★ 길이 460 이하 → getInfo 안전

// print("Date list length:", dateListClient.length);


// // ======================================================================
// // 4. client-side forEach로 safe export 생성 (Literal Task Names)
// // ======================================================================

// dateListClient.forEach(function(t, idx){

//   var date = ee.Date(t).format("YYYYMMdd").getInfo();  
//   // ↑ 여기서 완전한 문자열로 변환됨 → Task name에 100% 사용 가능

//   var img = allLST.filterDate(ee.Date(t), ee.Date(t).advance(1, 'day')).first();

//   // reduceRegions
//   var fc = img.reduceRegions({
//     collection: regions,
//     reducer: ee.Reducer.first(),
//     scale: 1000,
//     crs: img.projection()
//   }).map(function(f){
//     return f.set("date", date);
//   });

//   // Export (LITERAL STRING!)
//   Export.table.toDrive({
//     collection: fc,
//     description: 'LST_' + date,
//     fileNamePrefix: 'LST_' + date,
//     folder: 'GEE_LST_EXPORT',
//     fileFormat: 'CSV'
//   });

// });

// print("All EXPORT tasks created.");
