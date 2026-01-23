dyn.load("/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15")
dyn.load("/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100")
dyn.load("/opt/ohpc/pub/apps/gdal/3.5.1/lib/libgdal.so.31")




source("_targets.R")


bioatmo <- open.nc("/Net/Groups/BGI/work_2/EuropeanPHI/countrydatacubes/level_3_quarter.nc") |> read.nc()
eurostat_demo_r_d3dens <- open.nc("data/eurostat-nc/demo_r_d3dens.nc") |> read.nc()


# demo data

data(va3way)
dim(va3way)
va3way[va3way == 0] <- 0.001

res <- Tucker3(va3way)
plot(res, which = "tjplot")

va3way[, , 1]
va3way[, , 2]


# random data iwith our dims

countries <- c("DE", "AT", "BE", "CH", "AL")
groups <- c("bio1", "bio2", "soc1", "soc3", "soc3", "at1", "at2", "at3", "at4")
time_points <- c("2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4", "2021-Q1")

set.seed(123) # For reproducibility
array_data <- array(runif(length(countries) * length(groups) * length(time_points)),
    dim = c(length(countries), length(groups), length(time_points)),
    dimnames = list(Country = countries, Group = groups, Time = time_points)
)
array_data
cca <- Tucker3(array_data)
plot(cca, which = "tjplot")
