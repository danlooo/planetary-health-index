devtools::install_github("AckerDWM/gg3D")

# X: Biosphere, Y: Atmosphere, Z: Socioeconomics; we can use dim 1 first, and ignore 2 and 3 in the first try

library(tidyverse)
library(gg3D)
library(gganimate)

events <- tribble(
  ~from, ~to, ~event,
  2003, 2012, "one of the fastest-growing major economies in the world",
  2012, 2012, "Forest conservation code",
  2014, 2014, "Brazilian economic crisis",
  2015, 2016, "Warmest EI Nino-induced severe drought",
  2018, 2024, "increased deforestation alerts",
  2020, 2021, "COVID-19"
) |>
  mutate(country = "BRA") |>
  mutate(year = map2(from, to, ~ seq(.x, .y))) |>
  unnest_longer(year) |>
  select(-from, -to)

data <- 
  list.files("data", full.names = TRUE) |>
  map(read_csv) |>
  bind_rows() |>
  pivot_longer(starts_with("CC")) |>
  mutate(
    index_type = name |> str_extract("[XYZ]") |> recode("X" = "Biosphere", "Y" = "Atmosphere", Z = "Socioeconomics"),
    cca_axis = name |> str_extract("[0-9]") |> map_chr(~ str_glue("CCA{.x}"))
  ) |>
  select(-name) |>
  rename(country = `Country Code`) |>
  left_join(events) |>
  mutate(during_event = ! is.na(event)) |>
  arrange(country, year)

#
# 3D static
#

data |>
  pivot_wider(names_from = index_type, values_from = value) |>
  mutate(year = year |> map_int(~ ifelse(.x %% 5 == 0, .x, NA))) |> # display only every 5th year
  filter(cca_axis == "CCA1") |>
  filter(country == "BRA") |>
  ggplot(aes(x = Biosphere, y = Atmosphere, z = Socioeconomics, color = during_event, label = year)) +
  axes_3D() +
  stat_3D(geom="path") +
  stat_3D(geom="point") +
  stat_3D(geom="text", color = "black", size = 3) +
  labs_3D(labs=c("Bio", "Atmo", "Soc")) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "grey")) +
  facet_wrap(~country) +
  theme_void()

#
# Dim1 Dim2
#

data2 <-
  data |>
  pivot_wider(names_from = cca_axis, values_from = value) |>
  filter(country == "BRA")

current_year <- 2010

data2 |>
  ggplot(aes(CCA1, CCA2)) +
    geom_path(arrow = arrow(length = unit(3, "mm"))) +
    geom_point(data = ~ filter(.x, year == current_year), color = "darkred", size = 3) +
    facet_wrap(~index_type) +
    theme_minimal() +
    labs(
      title = str_glue("Brazil {current_year}"),
      subtitle = data2 |> filter(year == current_year) |> pull(event) |> unique() |> paste(collapse = "\n")
    )

#
# 3D dynamic
#


#
# 3D static
#


#
#  3D animation
#

plt <-
 data |>
  ggplot(aes(x=CCX1, y=CCX2, z=CCX3, color=`Country Code`)) + 
  theme_void() +
  transition_time(year) +
  ease_aes('linear') +
  axes_3D() +
  stat_3D() +
  labs_3D() +
  labs(x = "CC 1", y = "CC 2", z = "CC 3", title = "Year {as.integer(frame_time)}")

animate(plt, renderer = ffmpeg_renderer())
anim_save("gg3d-gganimate.webm")

#
# viz derivative: Change in CCA values over time
#

data |>
  arrange(`Country Code`, year) |>
  mutate(derivative = abs(CCZ1 - lag(CCZ1))) |>
  ggplot(aes(year, derivative)) +
    geom_line() +
    labs(x = "Time", y = "Change in CCA")+
    theme_minimal() +
    facet_wrap(~ `Country Code`)

data |>
  arrange(`Country Code`, year) |>
  mutate(derivative = CCZ1) |>
  ggplot(aes(year, derivative)) +
  geom_line() +
  labs(x = "Time", y = "Change in CCA")+
  theme_minimal() +
  facet_wrap(~ `Country Code`)

#
# just 2D
#

data |>
  pivot_longer(starts_with("CC")) |>
  mutate(
    index_type = name |> str_extract("[XYZ]"),
    cca_axis = name |> str_extract("[0-9]") |> map_chr(~ str_glue("CCA{.x}"))
  ) |>
  select(-name) |>
  pivot_wider(names_from = cca_axis, values_from = value) |>
  ggplot(aes(year, CCA1, color = index_type)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ `Country Code`)


