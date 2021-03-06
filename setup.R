library(sinew)

makeOxygen("quant_summary")
makeOxygen("datetime_df")
makeOxygen("complete_datetime")
makeOxygen("gg_quant")
makeOxygen("quant_smooth")
makeOxygen("roll_daily_median")

library(usethis)

use_package("dplyr", "imports")
use_package("rlang", "imports")
use_testthat()
use_roxygen_md()
use_cran_badge()
use_lifecycle_badge("Experimental")
