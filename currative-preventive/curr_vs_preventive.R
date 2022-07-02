
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  googlesheets4,
  highcharter,
  here,
  directlabels,
  ggthemes,
  hrbrthemes,
  scales,
  ggforce
)

df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1OQSABH5vFd6GjAXIxo3FRCiZOnnETrOzJh3apjAQ8xY",
  skip = 1
  ) %>%
  rename(
    No.Regeneration = 2
  ) %>%
  rename_all(
    ~str_replace(., " ", ".")
  ) %>%
  mutate(
    across(
      .cols = everything(),
       ~str_replace(., ",", ".")
      ),
    across(
      .cols = everything(),
      ~str_replace(., "%", "")
    ),
    across(
      .cols = everything(),
      ~ as.numeric(.x)
    ),
   # Year = as.factor(Year)
  )



ggplot(
  data = df,
  aes(x = Year)
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df)[2])
    ),
    alpha = 1,
    color = "darkblue",
    fill = "darkorange",
    size = 1,
  ) +
  geom_area(
    aes(
      y = Regeneration.1
      ),
    color = "#658e64",
    fill = "#658e64",
    size = 1,
    alpha = 0.65,
    ) +
  geom_area(
    aes(
      y = get(names(df)[4])
    ),
    alpha = 0.60,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      y = get(names(df)[5])
    ),
    alpha = 0.55,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      y = get(names(df)[6])
    ),
    alpha = 0.50,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      y = get(names(df)[7])
    ),
    alpha = 0.45,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df)[8])
    ),
    alpha = 0.40,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df)[9])
    ),
    alpha = 0.35,
    color = "red",
    fill = "red",
    size = 1
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df)[10])
    ),
    alpha = 0.30,
    color = "blue",
    fill = "blue",
    size = 1
  ) +
  scale_x_continuous(
    breaks = c(0:17)
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100, 125)
    ) +
  geom_hline(
    yintercept = 75,
    color = "red",
    size = 0.5,
    alpha = 0.75
  ) +
  theme_ipsum(grid = "Y")# +
  geom_text(
    data = dfl,
    aes(
      x = Year,
      y = Capacity,
      label = group_by(Year),
      summarise() mutateCapacity
    )

  )






dfl <- pivot_longer(
  df,
  cols = c(2, starts_with("Reg")),
  names_to = "Regeneration",
  values_to = "Capacity"
) %>%
  mutate(
    #Year = as.factor(Year),
    #Regeneration = as.factor(Regeneration)
  )

dfl$Regeneration <- factor(dfl$Regeneration, levels = c(
  "No.Regeneration", "Regeneration.1", "Regeneration.2", "Regeneration.3", "Regeneration.4",
  "Regeneration.5", "Regeneration.6", "Regeneration.7", "Regeneration.8")
  )

labels <- dfl %>%
  mutate(
    Year = floor(Year)
  ) %>%
  select(Year, Capacity) %>%
  group_by(Year) %>%
  summarise(Capacity = max(Capacity, na.rm = TRUE ))


colors <- c(
  # "#d90368", "#25382d", "#344e41", "#46684c", "#658e64", "#b2ceb3", "#152219", "#25382d", "#344e41"
  "#0000a3", "#002800", "#004900", "#006a00", "#008a00", "#00ab00", "#00cc00", "#00ec00", "#0eff0e"
  )


alphas <- c("#0.9", paste("#", seq(0.7, 0.35, by=-0.05), sep = ""))







ggplot(
  data = dfl,
  aes(
    x = Year,
    y = Capacity,
    )
  ) +
  geom_area(
    aes(
      color = Regeneration,
      fill = Regeneration
     ),
    size = 1,
    alpha = 0.5,
    position = "dodge",
  ) +
  scale_x_continuous(
    limits = c(0, 17),
    breaks = breaks_width(2),
    ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 50, 75, 100),
    #labels = paste(Capacity, "%")
  ) +
  geom_hline(
    yintercept = 75,
    color = "red",
    size = 0.5,
    alpha = 0.75
  ) +
  # geom_text(
  #   data = labels,
  #   aes(
  #     x = Year,
  #     y = Capacity,
  #     label = Capacity,
  #     ),
  #   nudge_y = 10
  #   ) +
  scale_color_manual(
    values = colors
  ) +
  scale_fill_manual(
    values = colors
  ) +
  scale_alpha_manual(
    values = alphas,
    #guide=F
    ) +
  labs(
    x = "Years of battery life",
    y = "Battery capacity",
    title = "Preventive regeneration",
    subtitle = "Regeneration as a battery maintenance operation"
  ) +
  theme_ipsum(
    grid = "Y"
    ) +
  ggsave(

  )








