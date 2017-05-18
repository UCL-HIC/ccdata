context("Testing deltaTime")
test_that("convert to delta time", {

    DAICU <- stname2code("DAICU")
    HeartRate <- stname2code("h_rate")
    e1 <- list()
    # set heart rate delta time for 1 hour.
    e1[[DAICU]] <- "2014-02-01T16:00:00"
    e1[[HeartRate]] <- data.frame(time="2014-02-01T17:00:00", val="80")
    e1[["item_1d"]] <- "xxxx"
    rc <- ccRecord() + new.episode(e1) 

    drc <- deltaTime(rc)

    expect_equal(rc@nepisodes, 1)
    expect_equal(drc@nepisodes, 1)

    

    # delta time should be 1 hour and the rest should be the same. 
    expect_equal(as.numeric(drc@episodes[[1]]@data[[HeartRate]]$time), 1)
    expect_equal(as.character(drc@episodes[[1]]@data[[HeartRate]]$val), "80")
    expect_equal(drc@episodes[[1]]@data[["item_1d"]], "xxxx")

    expect_error(deltaTime(ccRecord()))
})

test_that("remove episode while missing admission time", {

    e1 <- list()
    e1[["heart_rate"]] <- data.frame(time="2014-02-01T17:00:00", val="80")
    rc <- ccRecord() + new.episode(e1)
    expect_warning(rcd <- deltaTime(rc))
    expect_equal(rcd@nepisodes, 0)


})


test_that("with pseudotime flag, derive the admission time from 2d data", {

    e1 <- list()
    e1[["heart_rate"]] <- data.frame(time="2014-02-01T17:00:00", val="80")
    rc <- ccRecord() + new.episode(e1)
    rcd <- deltaTime(rc, pseudotime=T)
    expect_equal(rcd@nepisodes, 1)
    expect_equal(rcd@episodes[[1]]@data[["heart_rate"]]$time, 0)
})

test_that("episode has no time data", {
    e1 <- list()
    e1[["item_id"]] <- "xxxx"
    rc <- ccRecord() + new.episode(e1)
    expect_warning(rcd <- deltaTime(rc, pseudotime=T))
    expect_equal(rcd@nepisodes, 0)
})
