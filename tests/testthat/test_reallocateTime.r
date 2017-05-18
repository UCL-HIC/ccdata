context("Testing reallocate time variables")

test_that("unit test with data from 1-10",
{
   data <- data.frame(val=as.character(seq(10)), 
                      time=as.numeric(seq(10)))
   data_new <- reallocateTime(data, 10, 1)

   expect_equivalent(c(0:10), as.numeric(data_new$time))
   expect_equivalent(as.character(c("NA", seq(10))), as.character(data_new$val))

   data_new <- reallocateTime(data, 100, 0.1) 
   expect_equal(length(unique(data_new$val)), 11)
})

test_that("input and output data types", 
{
    data <- data.frame(seq(10), seq(10))
    expect_error(reallocateTime(data, 10, 1))

    data <- data.frame(val=seq(10), time=seq(10))
    expect_error(reallocateTime(data, 10, 1))
    
    data$val <- as.character(data$val)
    data$time <- as.character(data$time)
    expect_error(reallocateTime(data, 10, 1))

    data$val <- as.numeric(data$val)
    data$time <- as.numeric(data$time)
    out <- reallocateTime(data, 10, 1)

    data$val <- as.character(data$val)
    out <- reallocateTime(data, 10, .1)
}) 

test_that("check when item t_admission or t_discharge is missing", 
{
    # pseudotime delta time used the same mechenism that searching first and
    # last data time stamp.
    dt <- deltaTime(ccd, pseudotime=TRUE)
    ta <- dt@episodes[[1]]@t_admission
    td <- dt@episodes[[1]]@t_discharge
   
    rr <- reallocateTimeRecord(dt)
    time <- rr@episodes[[1]]@data[["NIHR_HIC_ICU_0108"]]$time
   
    # modify admission  time to NA
    dt@episodes[[1]]@t_admission <- as.POSIXct(NA)
    rt <- reallocateTimeRecord(dt)
    time_with_null <- rt@episodes[[1]]@data[["NIHR_HIC_ICU_0108"]]$time
    expect_equal(max(time_with_null), max(time))

    # modify discharge time to NA
    dt@episodes[[1]]@t_discharge <- as.POSIXct(NA) 
    rt <- reallocateTimeRecord(dt)
    time_with_null <- rt@episodes[[1]]@data[["NIHR_HIC_ICU_0108"]]$time
    expect_equal(max(time_with_null), max(time))

})

test_that("check data with meta data column (C++ function)", {
    input <- data.frame(time=seq(1, 100, 10), 
               val=rep("v", 10), 
               meta=rep("m", 10))
    output <- reallocateTime(input, 100, 1)
    # index increase by 1 
    expect_equal(nrow(output), 101)
    expect_equivalent(as.character(output[seq(2, 100, 10), 2]), rep("v", 10))
    expect_equivalent(as.character(output[seq(2, 100, 10), 3]), rep("m", 10))

    output <- reallocateTime(input, 100, 10)
    expect_equal(nrow(output), 11)

    output <- reallocateTime(input, 100, 0.1)
    expect_equal(nrow(output), 1001)
})
