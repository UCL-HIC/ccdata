context("Testing SQL functionalities")





test_that("Export SQL variable tables from ccRecord", {
   expect_error(rec2tb('nonsense'))
   expect_true("data.frame" %in% class(rec2tb(ccd, "h_rate")))

   ltb <- export.lontb(ccd)
   expect_equal(class(ltb), "list")
   expect_true(all(stname2code(names(ltb)) != "NA"))
})


test_that("Create SQLite database", {
    suppressWarnings(sql.create.database(ccd, "test_db"))
    file.remove('test_db')
})


test_that("", {
    dbname <- "Test.sqlite"
    if (file.exists(dbname))
        file.remove(dbname)

    db <- src_sqlite(dbname, create=TRUE)
    fattb <- data.frame(site_id = c(rep("s1", 5), rep("s2", 5)), 
                        episode_id = c(rep(1, 5), rep(1, 5)), 
                        time = c(seq(5), seq(5)))

    vartb <- cbind(fattb, h_rate=seq(1, 10))
    copy_to(db, fattb, 'fattb', temporary=FALSE)
    copy_to(db, vartb, 'vartb', temporary=FALSE)

    print(sql.collect.vartb(db, 'vartb'))
    left.join.var.table(db$path)
    print(sql.collect.vartb(db, 'fattb'))

    vartb <- cbind(fattb, blood_pres=seq(1, 10))
    copy_to(db, vartb, 'vartb', temporary=FALSE)
    left.join.var.table(db$path)
    print(data.frame(sql.collect.vartb(db, 'fattb')))

    file.remove(dbname)
})

test_that("create fat table", {

    dbname <- "Test.sqlite"
    suppressWarnings(sql.create.database(ccd, dbname))

    if (file.exists(dbname))
        file.remove(dbname)

    db <- src_sqlite(dbname, create=TRUE)

    
    h_rate <- data.frame(site_id = c(rep("s1", 5), rep("s2", 5)), 
                        episode_id = c(rep(1, 5), rep(1, 5)), 
                        time = c(seq(5), seq(5)))


#    create.fat.table(db, 1)
    file.remove(dbname)


})
