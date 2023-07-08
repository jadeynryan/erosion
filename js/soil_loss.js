/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var columbiaPlateau = ee.FeatureCollection("projects/ee-erosion/assets/ColumbiaPlateau"),
    soils = ee.Image("projects/ee-erosion/assets/soils"),
    k = ee.Image("projects/ee-erosion/assets/soil_roughness_factor"),
    slrc = ee.Image("projects/ee-erosion/assets/slrcImg"),
    modis = ee.ImageCollection("MODIS/061/MOD13Q1"),
    rtma = ee.ImageCollection("NOAA/NWS/RTMA"),
    airDensity = ee.Image("projects/ee-erosion/assets/USA_air-density_10m"),
    gldas = ee.ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
// ***********************************************************
// Calculate Revised Wind Erosion Equation (RWEQ)
// Jadey Ryan | July 1, 2023
// ***********************************************************

//////////////////////////////////////////////////////////////
// Columbia Plateau Boundary
//////////////////////////////////////////////////////////////

// Style
var cp_style = {color: 'black', fillColor: '#00000000', width: 1};

//Display the shapefile in the interactive map
Map.addLayer(columbiaPlateau.style(cp_style), 
  null, 
  'Columbia Plateau Boundary');

//Display the view to the center of the screen and scale the view
Map.centerObject(columbiaPlateau, 7.5);

// Function to clip images to Columbia Plateau
var clipToCP = function(image) {
  return image.clip(columbiaPlateau);
};

//////////////////////////////////////////////////////////////
// Palettes and batch export tool
//////////////////////////////////////////////////////////////

var palettes = require('users/gena/packages:palettes');
var batch = require('users/fitoprincipe/geetools:batch');

//////////////////////////////////////////////////////////////
// Date range
//////////////////////////////////////////////////////////////

// Set date range
var startDate = ee.Date('2018-01-01');
var endDate = ee.Date('2019-01-01'); // End date is exclusive

// Use these variables to iterate through years and months
// as seen in slrc and wf calculations.
var months = ee.List.sequence(1, 12);
var years = ee.List.sequence(
  startDate.get('year'), 
  endDate.get('year').subtract(1)); 

// Note: endDate is exclusive in filterDate() but not in 
// ee.Filter.calendarRange()
// This is why I subtract 1 from endDate in the years sequence.

//////////////////////////////////////////////////////////////
// Erodible fraction (EF) and soil crust factor (SCF)
//////////////////////////////////////////////////////////////

// Data acquired and processed in R 
// (https://github.com/jadeynryan/erosion/blob/main/R/soils.R). 

var efImg = soils.select('b8').rename('ef').clamp(0, 1);
var scfImg = soils.select('b9').rename('scf');

Map.addLayer(efImg, 
  {min: 0, max: 1, palette: palettes.colorbrewer.Reds[9]}, 
  'erodible fraction', 
  false);
  
Map.addLayer(scfImg, 
  {min: 0, max: 1, palette: palettes.colorbrewer.PuBuGn[9]}, 
  'soil crust factor',
  false);

//////////////////////////////////////////////////////////////
// Soil roughness factor (K')
//////////////////////////////////////////////////////////////

// Image acquired and processed in users/jadeynryan/erosion/k

var kImg = k;
Map.addLayer(kImg, 
  {min: 0.5, max: 1,   palette: palettes.colorbrewer.BuPu[9]}, 
  'soil roughness factor',
  false);
  
//////////////////////////////////////////////////////////////
// Combined crop factor (C)
//////////////////////////////////////////////////////////////

// Adapted from https://gis.stackexchange.com/questions/387012/google-earth-engine-calculating-and-plotting-monthly-average-ndvi-for-a-region

// Mask poor quality pixels and water
// Adapted from https://spatialthoughts.com/2021/08/19/qa-bands-bitmasks-gee/
var mask = function(image) {
  // Helper function to extract the values from specific bits
  // The input parameter can be a ee.Number() or ee.Image()
  // Code adapted from https://gis.stackexchange.com/a/349401/5160
  var bitwiseExtract = function(input, fromBit, toBit) {
    var maskSize = ee.Number(1).add(toBit).subtract(fromBit);
    var mask = ee.Number(1).leftShift(maskSize).subtract(1);
    return input.rightShift(fromBit).bitwiseAnd(mask);
  };
  
  var qaDetail = image.select('DetailedQA');
  var qaSummary = image.select('SummaryQA');
  var ndvi = image.select('NDVI');
  
  // QA Summary band = 0 is highest quality
  var qaMask = qaSummary.lte(0);
  // Bits 11 - 13 = 1 means nothing but land
  var waterMask = bitwiseExtract(qaDetail, 11, 13).eq(1);
  // NDVI values less than 0 are water or clouds
  var gtZeroMask = ndvi.gte(0);
  // Combine masks
  var mask = qaMask.and(waterMask).and(gtZeroMask);

  return image.updateMask(mask);
};

// Filter to date range, mask poor quality and water pixels, clip to CP
var ndviImgCol = modis.filterDate(startDate, endDate)
  .map(mask)
  .map(clipToCP)
  .select('NDVI');
  
// Map filtering and reducing across year-month combinations 
// and convert to ImageCollection
var ndviMeanYrMoImgCol = ee.ImageCollection.fromImages(
  years.map(function (y) {
        return months.map(function (m) {
            var startDate = ee.Date.fromYMD(y, m, 1);
            var endDate = startDate.advance(1, 'month');
            return ndviImgCol.filterDate(startDate, endDate)
              .mean().multiply(0.0001) // Apply scale factor
              .rename('ndvi')
              .set('year', y)
              .set('month', startDate.format('MM'))
              .set('yearMonth', startDate.format('YYYY-MM'));
        });
    }).flatten());

// MODIS example NDVI visualization palette
var ndviVis = {
  min: 0.1,
  max: 0.8,
  palette: [
    'ffffff', 'ce7e45', 'df923d', 'f1b555', 'fcd163', '99b718', '74a901',
    '66a000', '529400', '3e8601', '207401', '056201', '004c00', '023b01',
    '012e01', '011d01', '011301'
  ],
};

Map.addLayer(ndviMeanYrMoImgCol, ndviVis, 'ndvi', false);

// Borrelli et al. 2017 used SLRc = e^-5.614(cc^0.7366) from Fryrear et al. (2000)
// SLRc is the canopy cover soil loss ration relationship
// To simplify the model, only using SLRc, not SLRf or SLRs
// SLRf (soil loss ratio for flat cover) and SLRs (soil loss ratio for plant silhouette)
// Trout et al. 2008 found correlation between NDVI and CC
// Using NDVI as proxy for CC. Linear relationship is valid to a CC of 0.8, 
// as NDVI begins losing sensitivity to effective full cover.
var slrcImgCol = ndviMeanYrMoImgCol.map(function(image) {
  var ccPower = image.select('ndvi')
    .pow(0.7366)
    .multiply(-5.614);
  var slrcImg = ccPower.exp()
    .rename('slrc');
  return slrcImg.copyProperties(image);
});

Map.addLayer(slrcImgCol, 
  {min: 0, max: 0.3,   palette: palettes.colorbrewer.YlGn [9].reverse()}, 
  'slrc',
  false);
  
// Export all images to Google Drive
batch.Download.ImageCollection.toDrive(
  ee.ImageCollection(slrcImgCol),
  'erosion',
  {name: 'slrcImgCol_{yearMonth}',
   scale: 250,
   region: columbiaPlateau
  });
  
//////////////////////////////////////////////////////////////
// Weather factor (WF)
//////////////////////////////////////////////////////////////

////**** NOAA RTMA Wind Speed Calculations

// Filter to date range, select wind speed, clip to CP
var hourlyWindImgCol = rtma.filterDate(startDate, endDate)
  .select('WIND')
  .map(clipToCP);
  
// View wind speed
var windSpeedVis = {
  min: 0.0,
  max: 12.0,
  palette: ['001137', '01abab', 'e7eb05', '620500'],
};

Map.addLayer(hourlyWindImgCol, windSpeedVis, 'Wind Speed (m/s)', false);

// Function to calculate the wind part of the WF factor
var calculateWind = function(image) {
  var hourlyWind = image.select('WIND');
  
  // Create new image with wind speeds < 5 m/s are 0 
  // and >= 5 m/s are 1
  var hourlyWindExceedThreshold = hourlyWind.gt(5);
  var hourlyWindImg = ee.Image(hourlyWindExceedThreshold)
    .rename('hourlyWindExceedThreshold');
    
  // Calculate and create new band for u2(u2-ut)^2
  var hourlyWindFactor = hourlyWind.subtract(5)
    .pow(2)
    .multiply(hourlyWind)
    .multiply(hourlyWindExceedThreshold); // 0 if below threshold
  
  hourlyWindImg = hourlyWindImg.addBands(
    hourlyWindFactor.rename('hourlyWindFactor'));
  
  return hourlyWindImg;
};
  
// Map filtering and reducing across year-month combinations 
// and convert to ImageCollection
var windImgCol = ee.ImageCollection.fromImages(
  years.map(function (y) {
        return months.map(function (m) {
            var startDate = ee.Date.fromYMD(y, m, 1);
            var endDate = startDate.advance(1, 'month');
            var nDays = endDate.difference(startDate, 'days');
            return hourlyWindImgCol.filterDate(startDate, endDate)
              .map(calculateWind)
              .sum()
              .rename('excThr', 'sumWindFactor')
              .set('year', y)
              .set('month', startDate.format('MM'))
              .set('yearMonth', startDate.format('YYYY-MM'))
              .set('nDays', nDays);
        });
    }).flatten());

Map.addLayer(windImgCol, null, 'windImgCol', false);

////**** Global Wind Atlas
// Downloaded from https://globalwindatlas.info/api/gis/country/USA/air-density/10
// 250 m as modeled in WasP 12 from 10 year period
// Air density in kg/m^3
var airDensityImg = airDensity.clip(columbiaPlateau)
  .rename('airDensity');
  
Map.addLayer(airDensityImg, {min: 1, max: 1.25}, 'air density', false);  

////**** Gravity constant
// Create image with gravity as constant value
var gImg = ee.Image.constant(9.80665 ).rename('g');

////**** NASA GLDAS V2.1 
// 3-hourly 0.25 deg x 0.25 deg
// Snow depth in mm (instantaneous every 3 hours)
// Evapotranspiration in kg/m^2/s (backward 3-hour average)
// Rain precipitation rate in kg/m^2/s (backward 3-hour average)
// Wind speed at 2 m in m/s (instantaneous every 3 hours)
var gldasImgCol = gldas.filterDate(startDate, endDate)
  .select('SnowDepth_inst|Evap_tavg|Rainf_tavg')
  .map(clipToCP);

//** Soil wetness factor
// SW = [ET - R(Rd/Nd)]/ET
// This model ignores irrigation. If I had irrigation data, 
// the equation would be SW = [ET - ((R + I) * (Rd/Nd))]/ET

  // Convert to mm
  // 1 kg/m^2 rainwater on a surface = 1 mm thickness
var convertEtR = function(image) {
  // 3 hour = 10,800 seconds
  var et = image.select('Evap_tavg').multiply(10800).rename('et');
  var r = image.select('Rainf_tavg').multiply(10800).rename('r');

  return ee.Image([et, r]);
};

// Get daily sum of et and r
var dailyEtRImgCol = ee.ImageCollection.fromImages(
  years.map(function (y) {
        return months.map(function (m) {
            var startDate = ee.Date.fromYMD(y, m, 1);
            var endDate = startDate.advance(1, 'month');
            var nDays = endDate.difference(startDate, 'days');
            return ee.List.sequence(0, nDays.subtract(1)).map(function(dayOffset) {
              var start = startDate.advance(dayOffset, 'days');
              var end = start.advance(1, 'days');
              return gldasImgCol.filterDate(start, end)
                .map(convertEtR)
                .sum()
                .set('year', y)
                .set('month', startDate.format('MM'))
                .set('yearMonth', startDate.format('YYYY-MM'))
                .set('nDays', nDays);
              });
        });
    }).flatten());
    
// Count how many days r > 0
// dayR = 1 when r > 0
var calculateDayR = function(image) {
  var r = image.select('r');
  var mask = r.eq(0);
  var rd = mask.multiply(0).add(mask.not().eq(1));
  var rdImg = image.addBands(
    rd.rename('rd'));
  return rdImg;
};

var dailyEtRRdImgCol = dailyEtRImgCol.map(calculateDayR);

Map.addLayer(dailyEtRImgCol, null, 'dailyEtRImgCol', false);

// Get monthly sum of et and r and rd
var etRRdImgCol = ee.ImageCollection.fromImages(
  years.map(function (y) {
        return months.map(function (m) {
            var startDate = ee.Date.fromYMD(y, m, 1);
            var endDate = startDate.advance(1, 'month');
            var nDays = endDate.difference(startDate, 'days');
              return dailyEtRRdImgCol.filter(
                ee.Filter.eq('month', startDate.format('MM')))
                .sum()
                .set('year', y)
                .set('month', startDate.format('MM'))
                .set('yearMonth', startDate.format('YYYY-MM'))
                .set('nDays', nDays);
              });
    }).flatten());

Map.addLayer(etRRdImgCol, null, 'etRRdImgCol', false);

// Calculate SW
var swImgCol = etRRdImgCol.map(function(image) {
  var nDays = ee.Number(image.get('nDays'));
  
  return image.expression({
    expression: '(et - (r * (rd / nd))) / et',
    map: {
      et: image.select('et'),
      r: image.select('r'),
      rd: image.select('rd'),
      nd: nDays
    }
    })
    .clamp(0,1) // Clamp so negative values are changed to 0
    .rename('sw')
    .copyProperties(image);
});

Map.addLayer(swImgCol, null, 'swImgCol', false);

//** Snow cover factor
  
// Calculate the sd, which is the snow cover factor (dimensionless). 
// sd is equal to 1 when there is less than 0.0254 m of snow.
var calculateSd = function(image) {
  var snowDepth = image.select('SnowDepth_inst');
  var mask = snowDepth.gte(0.0254);
  var sd = mask.multiply(0).add(mask.not().multiply(1));
  var sdImg = image.addBands(
    sd.rename('sd'));
  return sdImg;
};

// Map to image collection then
// aggregate sd to mean of month
// Rough estimate of probabilty of snow since the mean is 
// the number of days there was snow > 0.0254 m divided by 
// the number of days in the month

var sdImgCol = ee.ImageCollection.fromImages(
    years.map(function (y) {
        return months.map(function (m) {
            var startDate = ee.Date.fromYMD(y, m, 1);
            var endDate = startDate.advance(1, 'month');
            var nDays = endDate.difference(startDate, 'days');
          return gldasImgCol.filterDate(startDate, endDate)
            .map(calculateSd)
            .select('sd')
            .mean()
            .set('year', y)
            .set('month', startDate.format('MM'))
            .set('yearMonth', startDate.format('YYYY-MM'))
            .set('nDays', nDays);
            });
  }).flatten());

Map.addLayer(sdImgCol, null, 'sdImgCol', false);

////**** Merge all bands into one image collection to calculate WF
// Create join
var join = ee.Join.inner();

// Create filter to join by
var yearMonthFilter = ee.Filter.equals({
  leftField: 'yearMonth',
  rightField: 'yearMonth'
});

// Function to merge bands together after join
var mergeBands = function(row) {
  return ee.Image.cat(row.get('primary'), row.get('secondary'));
};

// Join collections (GEE only allows two at a time)
var windERRdImgCol = join.apply(windImgCol, etRRdImgCol, yearMonthFilter)
  .map(mergeBands);

var swSdImgCol = join.apply(swImgCol, sdImgCol, yearMonthFilter)
  .map(mergeBands);


// Now finally join the two joins
var allFactorsImgCol = join.apply(windERRdImgCol, swSdImgCol, yearMonthFilter)
  .map(mergeBands);

// Calculate WF
// WF = sw * sd * [(windSummation * windThresholdExceedanceNum * airDensity) / (nd * g)]
var wfImgCol = allFactorsImgCol.map(function(image) {
  var img = ee.Image(image);
  var nDaysInPeriod = ee.Number(img.get('nDays'));
  var rho = airDensityImg.select('airDensity');
  var g = gImg.select('g');

  return img.expression({
    expression: 'sw * sd * ((u * ue * rho) / (nd * g))',
    map: {
      sw: img.select(['sw']),
      sd: img.select(['sd']),
      u: img.select(['sumWindFactor']),
      ue: img.select(['excThr']),
      rho: rho,
      nd: nDaysInPeriod,
      g: g
    }
  }).rename('wf')
    .copyProperties(image);
});

// Add layers to map
Map.addLayer(ee.ImageCollection(wfImgCol), null, 'wf', false);

// Export all images to Google Drive
batch.Download.ImageCollection.toDrive(
  ee.ImageCollection(wfImgCol),
  'erosion',
  {name: 'WF_{yearMonth}',
   scale: 250,
   region: columbiaPlateau
  });

//////////////////////////////////////////////////////////////
// Product of all 5 factors (WF * EF * SCF * K' * C)
//////////////////////////////////////////////////////////////

// Use same join, filter, and function to join two time-series 
// factors (WF and SLRC) together as seen near the end of the 
// WF calculations.

// Join collections 
var wfCImgCol = join.apply(wfImgCol, slrcImgCol, yearMonthFilter)
  .map(mergeBands);

Map.addLayer(ee.ImageCollection(wfCImgCol), null, 'wfCImgCol', false);

// Calculate (WF * EF * SCF * K' * C)
var productImgCol = wfCImgCol.map(function(image) {
  var img = ee.Image(image);
  var ef = efImg.select('ef');
  var scf = scfImg.select('scf');
  var k = kImg.select('k');

  return img.expression({
    expression: 'wf * ef * scf * k * slrc',
    map: {
      wf: img.select(['wf']),
      ef: ef,
      scf: scf,
      k: k,
      slrc: img.select(['slrc'])
    }
  }).rename('product')
    .copyProperties(image);
});

Map.addLayer(ee.ImageCollection(productImgCol), null, 'product', false);

//////////////////////////////////////////////////////////////
// Critical field length (s)
// Maximum transport capacity (Qmax)
// Soil wind erosion potential (SWEP)
//////////////////////////////////////////////////////////////

// s = 150.71 * (WF * EF * SCF * K' * C)^(-0.3711)
// Qmax = 109.8 * (WF * EF * SCF * K' * C)
// swep = (2x/s^2) * qmax * e^-(x/s)^2

// x is the distance from the upwind edge of the field (m)

var soilLossImgCol = productImgCol.map(function(image) {
  var img = ee.Image(image).select('product');
  var s = img.pow(-0.3711).multiply(150.71).rename('s');
  var qmax = img.multiply(109.8).rename('qmax');
  var x = ee.Image.constant(55).rename('x');
  var exp = x.divide(s).pow(2).multiply(-1).exp();
  
  var swep = x.multiply(2)
    .divide(s.pow(2))
    .multiply(qmax)
    .multiply(exp)
    .rename('swep');
    
  return img.addBands(s)
    .addBands(qmax)
    .addBands(swep)
    .copyProperties(img);
});

print('soilLossImgCol', soilLossImgCol);

print('soilLossImgCol crs and resolution', 
  ee.ImageCollection(soilLossImgCol).first().projection().crs(), 
  ee.ImageCollection(soilLossImgCol).first().projection().nominalScale());

Map.addLayer(ee.ImageCollection(soilLossImgCol).select('swep'), 
  {min: 0, max: 45, palette: palettes.colorbrewer.YlOrBr[9]}, 
  'soilLoss');
  
// Export all images to Google Drive
batch.Download.ImageCollection.toDrive(
  ee.ImageCollection(soilLossImgCol),
  'erosion',
  {name: 'windSoilLoss_{yearMonth}',
   scale: 250,
   region: columbiaPlateau
  });
