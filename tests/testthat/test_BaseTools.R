library(rFUSION)

context("Tests for rFUSION base tools")

### PRE-PROCESS

  # Read input data
  inLAS <- file.path("testData", "SamplePlot.las")
  subAreas <- file.path("testData", "subAreas.shp")
  inTiles <- list.files("testData", pattern = "SampleTile", full.names = TRUE)

  # Create temporary folder
  tempFolder <- file.path(tempdir(), "rFUSIONtemp")
  dir.create(tempFolder)
  tempFile <- function(file) file.path(tempFolder, file)

  # Deactivate print-to-console
  oldPrintToConsole <- FUSIONprintToConsole()
  FUSIONprintToConsole(FALSE)

### APPLY PROCESSES

  GroundFilter(outputfile = tempFile("groundfilt.las"),
               cellsize = 1, datafile = inLAS, iterations = 8)

  GridSurfaceCreate(surfacefile = tempFile("ground.dtm"),
                    cellsize = 1,
                    datafile = tempFile("groundfilt.las"))

  DTM2ASCII(tempFile("ground.dtm"), tempFile("ground.asc"))

  ground <- raster::raster(tempFile("ground.asc"))

  CanopyModel(surfacefile = tempFile("canopy.dtm"),
              cellsize = 0.25,
              datafile = inLAS,
              gridxy = FUSIONExtentRounder(raster::extent(ground), cellSize = 0.25, align = "topleft"),
              ground = tempFile("ground.dtm"))

  DTM2ASCII(tempFile("canopy.dtm"), tempFile("canopy.asc"))

  PolyClipData(Polyfile = subAreas,
               Outputfile = tempFile("polyClips.las"),
               Datafile = inLAS,
               multifile = TRUE)

  polyClips <- list.files(tempFolder, full.names = TRUE, pattern = "polyClips")

  CloudMetrics(InputDataSpecifier = polyClips,
               OutputFileName = tempFile("cloudMetrics.csv"), above = 245,
               minht = 243, maxht = 248)

  cloudMetrics <- read.csv(tempFile("cloudMetrics.csv"))

  ClipData(InputSpecifier = inLAS,
           SampleFile = tempFile("clipSample.las"),
           437257.89, 5527200.25, 437260.80, 5527203.43)

  LDA2ASCII(InputFile = inLAS,
            OutputFile = tempFile("ascii.csv"),
            format = 0)

  GridMetrics(groundfile = tempFile("ground.dtm"),
              heightbreak = 1,
              cellsize = 2,
              outputfile = tempFile("gridmetrics.csv"),
              datafile = inTiles,
              outlier = c(0,30))

  CSV2Grid(inputfile = tempFile("gridmetrics_all_returns_elevation_stats.csv"),
           column = 7,
           outputfile = tempFile("gridmetrics_max.asc"))
  CSV2Grid(inputfile = tempFile("gridmetrics_all_returns_elevation_stats.csv"),
           column = 6,
           outputfile = tempFile("gridmetrics_min.asc"))

  gridmetrics.max <- raster::raster(tempFile("gridmetrics_max.asc"))
  gridmetrics.min <- raster::raster(tempFile("gridmetrics_min.asc"))

### DO TESTS

  test_that("Check that 'GroundFilter' created the expected ouput", {

    expect_true(file.exists(tempFile("groundfilt.las")))
    expect_equal(file.info(tempFile("groundfilt.las"))$size, 391797)
  })

  test_that("Check that 'GridSurfaceCreate' created the expected output", {

    expect_true(file.exists(tempFile("ground.dtm")))

    expect_equal(min(ground[], na.rm = TRUE), 243.0765, tolerance = 0.00001)
    expect_equal(max(ground[], na.rm = TRUE), 248.6490, tolerance = 0.00001)
  })

  test_that("Check that 'CanopyModel' created the expected output", {

    expect_true(file.exists(tempFile("canopy.dtm")))

    canopy <- raster::raster(tempFile("canopy.asc"))

    # Both rasters have same extent
    expect_identical(raster::extent(ground), raster::extent(canopy))

    expect_equal(min(canopy[], na.rm = TRUE), 0.00106, tolerance = 0.00001)
    expect_equal(max(canopy[], na.rm = TRUE), 3.361639, tolerance = 0.00001)
  })

  test_that("Check that 'CloudMetrics' created the expected output",{

    expect_equal(cloudMetrics[,"Total.return.count"], c(1350, 1174))
    expect_equal(cloudMetrics[,"Percentage.all.returns.above.245.00"], c(100, 92.3339))
    expect_equal(cloudMetrics[,"All.returns.above.mean"], c(731, 674))
  })

  test_that("Check that 'ClipData' created the expected ouput", {

    expect_true(file.exists(tempFile("clipSample.las")))
    expect_equal(file.info(tempFile("clipSample.las"))$size, 82873)
  })


  test_that("Check that 'LDA2ASCII' created the expected ouput", {

    expect_true(file.exists(tempFile("ascii.csv")))

    ascii <- read.csv(tempFile("ascii.csv"), sep = " ")

    expect_equal(as.numeric(ascii[1,]), c(437254.7, 5527201.71, 248.506))
    expect_equal(as.numeric(ascii[100,]), c(437254.904, 5527202.037, 248.285004))
    expect_equal(as.numeric(ascii[1000,]), c(437255.907, 5527203.123, 246.843))
  })


  test_that("Check that 'GridMetrics' created the expected output",{

    expect_equal(as.numeric(gridmetrics.max[6,4]), 3.3616, tolerance = 0.00001)
    expect_equal(as.numeric(gridmetrics.min[2,4]), 0.0027, tolerance = 0.00001)
  })

### CLEAN-UP

  unlink(tempFolder, recursive = TRUE)

  FUSIONprintToConsole(oldPrintToConsole)

