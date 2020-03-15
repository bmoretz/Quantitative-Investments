library(devtools)
library(pkgbuild)

find_rtools(T)

install.packages("devtools") # if not installed
install.packages("FinancialInstrument") #if not installed
install.packages("PerformanceAnalytics") #if not installed

devtools::install_github("braverock/blotter", args='--no-multiarch')

# next install blotter from GitHub
devtools::install_github("braverock/blotter")
# next install quantstrat from GitHub
devtools::install_github("braverock/quantstrat")

pkgbuild::check_build_tools(debug = TRUE)

devtools::find_rtools()

options(buildtools.check = function(action) TRUE )

devtools::install_github("r-lib/devtools")

Sys.setenv(BINPREF = "C:/Rtools/mingw_64/bin")

Sys.getenv("BINPREF")
Sys.getenv("PATH")

devtools::install_local(path = "D:/GitHub/blotter-master.zip")
devtools::install_local(path = "D:/GitHub/quantstrat-master.zip")

remotes::install_github("braverock/quantstrat")

Sys.getenv("R_HOME")

assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")

install.packages("pkgbuild") # pkgbuild is not available (for R version 3.5.0)
install.packages("devtools") # make sure you have the latest version from CRAN
library(devtools) # load package
devtools::install_github("r-lib/pkgbuild") # install updated version of pkgbuild from GitHub
library(pkgbuild) # load package
find_rtools() # should be TRUE, assuming you have Rtools 3.5

R.version

pkgbuild::has_rtools(debug = TRUE)

.libPaths()
