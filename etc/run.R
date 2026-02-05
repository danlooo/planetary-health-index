source("_targets.R")
tar_load_everything()

socio_features_data <-
    eurostat_data |>
    transmute(
        code,
        data = map2(
            data,
            code,
            possibly(
                otherwise = NA,
                ~ {
                    res <-
                        .x |>
                        # discard aggregated data
                        filter(!str_starts(geo, "EU|EA|EEA")) |> # e.g euro area
                        group_by(nut_level = nchar(geo) - 2) |>
                        nest() |>
                        arrange(-nut_level) |>
                        pull(data) |>
                        first() |>
                        # create variable
                        mutate(code = .y) |>
                        select(code, everything()) |>
                        unite(
                            var,
                            -any_of(setdiff(stable_columns, "code")),
                            sep = "_"
                        ) |>
                        distinct(var, geo, TIME_PERIOD, .keep_all = TRUE) |>
                        # re-scale
                        resample_time_to_quarter() |>
                        resample_space_to_nuts3(
                            nuts3_regions,
                            eurostat_regions
                        ) |>
                        # crop to bioatmo range
                        filter(TIME_PERIOD %in% times & geo %in% regions) |>
                        select(var, geo, TIME_PERIOD, values)
                }
            )
        )
    ) |>
    filter(!is.na(data)) |>
    head() |>
    unnest() |>
    count(code, var, geo) |>
    mutate(na_frac = 1 - (n / length(times))) |>
    filter(na_frac < 0.1) |>
    count(code, geo)

land_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

nuts3_sf |>
    inner_join(socio_features_data) |>
    ggplot(aes(geometry = geometry, fill = n)) +
    geom_sf(data = land_sf, fill = light_gray_color) +
    geom_sf() +
    scale_fill_viridis_c() +
    coord_sf(
        xlim = c(2377294, 7453440),
        ylim = c(1313597, 5628510),
        crs = 3035
    ) +
    facet_wrap(~code) +
    labs(
        title = "Available features",
        subtitle = "<10% NAs from 2001 to 2021"
    )


x |>
    filter(str_detect(geo, "PT"))

# NUTS geo changed e.g. for Baixo Alentejo from PT184 to PT1C2 in 2024

eurostat_data |>
    filter(code == "ilc_di11") |>
    unnest(data) |>
    mutate(l = length())


list(
    NUTS2024 = "data/nuts-regions/NUTS_RG_20M_2024_4326.geojson",
    NUTS2021 = "data/nuts-regions/NUTS_RG_20M_2021_4326.geojson"
) |>
    enframe(name = "nuts_version") |>
    mutate(value = read_sf(value))


st_join(
    read_sf("data/nuts-regions/NUTS_RG_20M_2024_4326.geojson") |>
        mutate(version = 2024),
    read_sf("data/nuts-regions/NUTS_RG_20M_2021_4326.geojson") |>
        mutate(version = 2021),
)

tibble() |>
    ggplot(
        aes(
            geometry = geometry,
            color = factor(version)
        ),
    ) +
    geom_sf(
        data = read_sf("data/nuts-regions/NUTS_RG_20M_2003_4326.geojson") |>
            filter(LEVL_CODE == 3) |>
            mutate(version = 2003),
    ) +
    geom_sf(
        data = read_sf("data/nuts-regions/NUTS_RG_20M_2024_4326.geojson") |>
            filter(LEVL_CODE == 3) |>
            mutate(version = 2024),
    ) +
    coord_sf(
        xlim = c(2377294, 7453440),
        ylim = c(1313597, 5628510),
        crs = 3035
    )


nuts2003_sf <- read_sf("data/nuts-regions/NUTS_RG_20M_2003_4326.geojson")
nuts2024_sf <- read_sf("data/nuts-regions/NUTS_RG_20M_2024_4326.geojson")


# Calculate intersection areas and join based on highest overlap
geo_overlap <- st_intersection(nuts2003_sf, nuts2024_sf) %>%
  mutate(intersection_area = st_area(geometry)) %>%
  group_by(geo2003 = nuts2003_sf$geo, geo2024 = nuts2024_sf$geo) %>%
  summarise(max_overlap = max(intersection_area, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(max_overlap))

# Create the tibble
overlap_tibble <- geo_overlap %>%
  select(geo2003, geo2024)
