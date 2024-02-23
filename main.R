devtools::install_github("AckerDWM/gg3D")

# X: Biosphere, Y: Atmosphere, Z: Socioeconomics; we can use dim 1 first, and ignore 2 and 3 in the first try

library(tidyverse)
library(gg3D)
library(gganimate)


bra_events <- tribble(
  ~from, ~to, ~description,
  2003,2012, "one of the fastest-growing major economies in the world",
  2012,2012, "Forest conservation code",
  2014, 2014, "Brazilian economic crisis",
  2015, 2016, "Warmest EI Nino-induced severe drought",
  2018, 2024, "increased deforestation alerts",
  2020, 2021, "COVID-19"
)

data <- 
  list.files("data", full.names = TRUE) |>
  map(read_csv) |>
  bind_rows() 

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


