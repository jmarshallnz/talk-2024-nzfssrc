library(tidymodels)

load("~/projects/dist_ranger/data/cgMLST_dat.RData")

sources <- cgMLST |> select(-LabID) |> filter(Source != "Human") |>
  mutate(across(everything(), droplevels))
humans  <- cgMLST |> select(-LabID) |> filter(Source == "Human") |>
  mutate(across(everything(), droplevels))

source_ids <- data.frame(Source = levels(sources$Source)) |>
  rowid_to_column('prediction')
library(ranger)
model <- ranger(Source ~ ., data=sources)
predict(model, data=humans, predict.all = TRUE) |>
  as.data.frame() |>
  pivot_longer(everything(), names_to='tree', values_to='prediction') |>
  left_join(source_ids) |>
  group_by(tree) |>
  count(Source) |>
  mutate(p = n/sum(n)) |>
  write_csv("data/forest/ranger_defaults.csv")

  ggplot() +
  geom_boxplot(aes(x=Source, col=Source, y=p)) |>
  guides(col='none') +
  scale_colour_manual(values = source_cols2)

rand_forest(mode='classification') |>
  fit(Source ~ ., data=sources) |>
  predict(new_data = humans) |>
  as.data.frame() |>
  ggplot() +
  geom_bar(aes(x=Source))
