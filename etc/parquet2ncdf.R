
dyn.load('/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15')
dyn.load('/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100')
dyn.load("/opt/ohpc/pub/apps/gdal/3.5.1/lib/libgdal.so.31")

library(ncdf4)
library(arrow)
library(dplyr)
library(tidyr)
library(tibble)

i_code <- "avia_gor_lt" # <---- no geo
i_code <- "demo_r_deaths"  # <---- no split needed


df <- read_parquet(paste0("out/eurostat-raw/", i_code, ".parquet")) %>% tibble()


stable_columns <- c("code", "freq", "unit", "geo", "TIME_PERIOD", "values")
columns_to_split_by <- setdiff(names(df), stable_columns)

# Split the dataframe
split_dfs <- df %>%
  mutate(code = i_code) %>%
  unite(col = descriptor, all_of(c("code", columns_to_split_by)), sep = "_", remove = TRUE) %>%
  group_by(descriptor) %>%
  nest()%>% deframe()


# predefine here the desired dimensions of the future ncdf datacube
# ideally these should be done based on the target dates and target NUTS regions
# and then the function after should populate these datasets accoridingly with 
# data that is available


# vals <-colnames(mat) %>% substr(start = 5, stop =10)) # names of the values
dim_NUTS <- ncdim_def(name = "NUTS_id", units = "id", vals = 1:dim(mat)[2])
dim_TIME <- ncdim_def(name = "time", units = "days since 1990-01-01",
                      vals = as.numeric(dat_wide$TIME_PERIOD - as.Date("1990-01-01")))



# function to get datasets into a ncdf
get_ncvar <- function(dataset_id) {
  
  dat_wide <- split_dfs[[dataset_id]] %>%
    pivot_wider(
      names_from = geo,
      names_prefix = "geo_",
      values_from = values
      ) %>% 
    arrange(by = TIME_PERIOD)
  
  mat <- dat_wide %>% select(starts_with("geo_")) %>% as.matrix()
  
  # # vals <-colnames(mat) %>% substr(start = 5, stop =10)) # names of the values
  # dim_NUTS <- ncdim_def(name = "NUTS_id", units = "id", vals = 1:dim(mat)[2])
  # dim_TIME <- ncdim_def(name = "time", units = "days since 1990-01-01",
  #                       vals = as.numeric(dat_wide$TIME_PERIOD - as.Date("1990-01-01")))
  
  nc_var <- ncvar_def(name = dataset_id, units = "", dim = c(dim_NUTS, dim_TIME))
  
  return(nc_var)
}

# this part needs to be adapted, maybe with purrr and map2 function
ncvars <- lapply(names(split_dfs),get_ncvar)

# this would create one netcdf for a given dataset, and if it has many
# subdatasets (because of factors other than the main columns), it would have
# several variables
nc_create(filename, ncvars)
