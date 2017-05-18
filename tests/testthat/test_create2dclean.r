context("Testing create2dclean")

pseudoepisode <- function(n) {
    ep <- list(NIHR_HIC_ICU_0108=data.frame(time=as.numeric(seq(10)), 
                                      val=seq(81, 90)), 
         NIHR_HIC_ICU_0002="site_i", 
         NIHR_HIC_ICU_0005="episode_i"
         )
    cr <- ccRecord()
    for (i in seq(n)) 
        cr <- cr + new.episode(ep)
    cr
}



test_that("test create2dclean2",
{
    config <- '../data/test_analysis_ref.yaml'
    cr <- pseudoepisode(5)

#    tb_1 <- create2dclean(cr, config, freq=1, 1)
#    for(i in c(3)) {
#        tb <- create2dclean2(cr, config, freq=1, nchunks=i)
#        expect_equal(tb_1$time, tb$time)
#        expect_equal(tb_1$site, tb$site)
#        expect_equal(tb_1$episode_id, tb$episode_id)
#        expect_equal(tb_1$NIHR_HIC_ICU_0108, tb$NIHR_HIC_ICU_0108)
#    }
})
