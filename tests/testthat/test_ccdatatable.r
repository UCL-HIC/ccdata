context("Testing ccTable")

test.record <- function(v, item) {
    cr <- ccRecord()
    for (i in seq(v)) {
        hr <- data.frame(time=as.numeric(seq(v[[i]])),
                         val=as.character(v[[i]]))
        lst <- list()
        lst[[item]] <- hr
        lst[[stname2code("ADNO")]] <- paste("episode_", i)
        lst[[stname2code("ICNNO")]] <- paste("site_", i)
        cr <- cr + new.episode(lst)
    }
    cr
}


test_that("test create table",{
    
    ccd_delta <- suppressWarnings(deltaTime(ccd, pseudotime=T))
    conf <- yaml.load_file('../data/test_2yml.yml')

    tb <- ccTable(record=ccd_delta, conf=conf)
    tb$create.table(freq=1)
    # assign table to both origin and clean table
    expect_true(!is.null(tb$torigin))
    expect_equivalent(tb$torigin, tb$tclean)
})

test_that("test get.missingness", {
    cr <-
        ccRecord()+new.episode(list(NIHR_HIC_ICU_0108=data.frame(time=as.numeric(seq(100)),
                                                       val=as.character(rep(10,100)))))
    tb <- ccTable(record=cr, conf=yaml.load_file('../data/test_2yml.yml'))
    tb$create.table(freq=1)
    tb$conf[[1]][['missingness']][['labels']][['yellow']] <- 1
    tb$get.missingness()
    expect_equal(tb$dquality$missingness$NIHR_HIC_ICU_0108.yellow, 100/101*100)

    tb$conf[[1]][['missingness']][['labels']][['yellow']] <- 0.1
    tb$get.missingness()
    expect_equal(tb$dquality$missingness$NIHR_HIC_ICU_0108.yellow, 100/1001*100)


    tclean <- tb$tclean
    # check the case when there is no missingness table
    tb$dquality$missingness <- data.table(NULL)
    tb$tclean <- data.table(NULL)
    tb$filter.missingness()
    expect_true(any(class(tb$tclean)=="data.table"))
    expect_equivalent(tclean, tb$tclean)
})


test_that("test filter missingness", 
{
    yaml <- "NIHR_HIC_ICU_0108:
  shortName: test_val
  dataItem: Heart rate
  missingness:
      labels:
          yellow: 1
      accept_2d:
          yellow: 70 
      impute_2d:
          lead: 3
          lag: 3
          fun: median
      apply: drop_episode"

    conf <- yaml.load(yaml)

    # 1) create single item episodes, v is a list of item values. 
    # 2) Run the missingness filter. 
    missingness_run <- function(v, item="NIHR_HIC_ICU_0108") {
        cr <- test.record(v, item)
        tb <- ccTable(record=cr, conf=conf)
        tb$create.table(freq=1)
        tb$filter.missingness()
        tb$apply.filters()
        expect_true(any(class(tb$tclean) == "data.table"))
        return(tb)
    }

    tb <- missingness_run(list(seq(10)), "nimporte_quoi") # hr not presented
    expect_equal(nrow(tb$tclean), 0)

    tb <- missingness_run(list(c(NA, NA, 1, NA))) # 20% present rate, t0 is NA as well.
    expect_equal(tb$dquality$missingness$NIHR_HIC_ICU_0108.yellow, 20)
    expect_equal(nrow(tb$tclean), 0)
    
    tb <- missingness_run(list(c(1, 1, 1))) # 75% present rate, t0 is NA as well.
    expect_equal(tb$dquality$missingness$NIHR_HIC_ICU_0108.yellow, 75)
    
    tb <- missingness_run(list(rep(NA, 10), seq(10)))# accept one and dump one. 
    expect_equal(nrow(tb$dquality$missingness), 2)
    expect_true(!"episode_ 1" %in% tb$tclean$episode_id)


})

test_that("test imputation", 
{

    yaml <- "NIHR_HIC_ICU_0108:
  shortName: test_val
  dataItem: Heart rate
  missingness:
      labels:
          yellow: 1
      accept_2d:
          yellow: 70 
      impute_2d:
          lead: 1
          lag: 1
          fun: median
      apply: drop_episode"

    conf <- yaml.load(yaml)
    
    imputation_run <- function(v, item="NIHR_HIC_ICU_0108") {
        cr <- test.record(v, item)
        tb <- ccTable(record=cr, conf=conf)
        tb$create.table(freq=1)
        tb$imputation()
        return(tb)
    }

    tb <- imputation_run(list(c(1,1,NA,1)))
    expect_true(all(tb$tclean$NIHR_HIC_ICU_0108 == 1))


    tb <- imputation_run(list(c(1,1,NA,1, NA, NA, NA, NA, 1)))
    expect_equivalent(tb$tclean$NIHR_HIC_ICU_0108, 
                      c(1, 1, 1, 1, 1, 1, NA, NA, 1, 1))


    tb <- imputation_run(list(c(1, 10, NA, 100)))
    expect_equivalent(tb$tclean$NIHR_HIC_ICU_0108,
                      c(1, 1, 10, median(c(10, 100)), 100))

    # On different aggregation functions 
    yaml <- "NIHR_HIC_ICU_0108:
  shortName: test_val
  dataItem: Heart rate
  missingness:
      impute_2d:
          lead: 1
          lag: 1
          fun: sum"

    conf <- yaml.load(yaml)

    tb <- imputation_run(list(c(1, 10, NA, 100)))
    expect_equivalent(tb$tclean$NIHR_HIC_ICU_0108,
                      c(1, 1, 10, sum(c(10, 100)), 100))

})
#
#
#test_that("test range check", 
#{
#    tb <- env$tb
#    tb$tclean <- tb$torigin
## case1 : no range specified in yml
## case2 : missing range speicification 
## case3 : overlapping, i.e. accept and impossible should not overlap.
#})
#
#
#test_that("test category data filter", 
#{
#    tb <- env$tb
#})
#
#
#
#test_that("test apply filter", 
#{
#    tb <- env$tb
#    tb$filter.ranges()
#    tb$filter.category()
#    tb$filter.missingness()
#    tb$filter.nodata()
#    tb$apply.filters()
#    tt <<- tb
#})
#
