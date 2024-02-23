devtools::install_github("AckerDWM/gg3D")

# X: Biosphere, Y: Atmosphere, Z: Socioeconomics; we can use dim 1 first, and ignore 2 and 3 in the first try

library(tidyverse)
library(gg3D)
library(gganimate)

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
# viz derivative
#

data |>
  arrange(`Country Code`, year) |>
  mutate(derivative = abs(CCX1 - lag(CCX1)) + abs(CCX2 - lag(CCX2))) |>
  ggplot(aes(year, derivative, color = `Country Code`)) +
    geom_line() +
    labs(x = "Time", y = "Change in CCA")+
    theme_minimal()
