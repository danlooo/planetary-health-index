#!/usr/bin/env R

# Improve speed of CCA by removing highly correlated features

library(targets)
library(tidyverse)
library(corrplot) 

tar_load(cube_tbl)
tar_load(features)

cube <-
  cube_tbl |>
  unite("space_time", geo, quarter, year) |>
  pivot_wider(names_from = var_id, values_from = value) |>
  column_to_rownames("space_time")

cube[is.na(cube)] <- 0

feature_cors <- cor(cube, method="pearson")
corrplot(feature_cors)

feature_clust <- hclust(as.dist(1-feature_cors))
feature_clusters <- cutree(feature_clust, h=0.5) 

feature_clusters |> unique() |> length()
plot(feature_clust)

features |>
  mutate(cluster = map_int(var_id, ~ feature_clusters[.x])) |>
  group_by(cluster) |>
  # prefer total features
  arrange(-str_detect(label, "TOTAL"), -str_detect(label, "T"), -str_detect(unit, "euro")) |>
  slice(1)
