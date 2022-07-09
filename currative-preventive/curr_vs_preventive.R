
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



# ******************************************************************************
# ******************************************************************************
# Preventive regeneration chart
# ******************************************************************************
# ******************************************************************************


# Load basic data for backup batteries
df_preventive_BB <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1OQSABH5vFd6GjAXIxo3FRCiZOnnETrOzJh3apjAQ8xY",
  sheet = "preventive-BB",
  range = "A2:J68",
  #skip = 1
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

# Save it as csv
write_csv(df_preventive_BB, here("currative-preventive", "data", "df_preventive_BB.csv"))


# Plot the basic df_preventive_BB
# Each column has own geom_area
ggplot(
  data = df_preventive_BB,
  aes(x = Year)
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df_preventive_BB)[2])
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
      y = get(names(df_preventive_BB)[4])
    ),
    alpha = 0.60,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      y = get(names(df_preventive_BB)[5])
    ),
    alpha = 0.55,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      y = get(names(df_preventive_BB)[6])
    ),
    alpha = 0.50,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      y = get(names(df_preventive_BB)[7])
    ),
    alpha = 0.45,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df_preventive_BB)[8])
    ),
    alpha = 0.40,
    color = "#658e64",
    fill = "#658e64",
    size = 1
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df_preventive_BB)[9])
    ),
    alpha = 0.35,
    color = "red",
    fill = "red",
    size = 1
  ) +
  geom_area(
    aes(
      #y = No.Regeneration
      y = get(names(df_preventive_BB)[10])
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
  theme_ipsum(grid = "Y")




# Create data for automatic plotting of each column
dfl_preventive_BB <- pivot_longer(
  df_preventive_BB,
  cols = c(2, starts_with("Reg")),
  names_to = "Regeneration",
  values_to = "Capacity"
)
# Save it as csv
write_csv(dfl_preventive_BB, here("currative-preventive", "data", "dfl_preventive_BB.csv"))

# Create data frame for printing labels - how much capacity battery has
labels_preventive_BB <- dfl_preventive_BB %>%
  mutate(
    Year = floor(Year)
  ) %>%
  select(Year, Capacity) %>%
  group_by(Year) %>%
  summarise(Capacity = max(Capacity, na.rm = TRUE ))

# Create colors scale for scale_color_manual()
colors_preventive_BB <- c(
  "#0000a3", "#002800", "#004900", "#006a00", "#008a00", "#00ab00", "#00cc00", "#00ec00", "#0eff0e"
  )

# Create alpha levels for scale_alpha()
alphas_preventive_BB <- c("#0.9", paste("#", seq(0.7, 0.35, by=-0.05), sep = ""))

# Guide labels
guide_labels_preventive_BB = c("NONE", "FIRST", "SECOND", "THIRD", "FOURTH", "FIFTH", "SIXTH", "SEVENTH", "EIGHT")


# Plot the chart
preventive_basic_BB <- ggplot(
  data = dfl_preventive_BB,
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
    "Years of battery use"
    ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 50, 75, 100),
    "Battery capacity [%]"
  ) +
  geom_hline(
    yintercept = 75,
    color = "red",
    size = 0.75,
    alpha = 0.75
  ) +
    geom_vline(
      xintercept = 10,
      color = "darkorange",
      size = 1,
      alpha = 0.75
    ) +
    annotate(
      "label",
      x = 10.5, y = 100,
      label = "Designed lifespan",
      size = 4,
      hjust = 0.065,
      fill = "orange",
      color = "white",
      fontface = "bold",
      alpha = 0.7
    ) +
  scale_color_manual(
    values = colors_preventive_BB,
    #labels = guide_labels_preventive_BB,
    guide = FALSE,
  ) +
  scale_fill_manual(
    values = colors_preventive_BB,
    labels = guide_labels_preventive_BB,
  ) +
  scale_alpha_manual(
    values = alphas_preventive_BB,
    #guide=F
    ) +
  theme_ipsum_rc(
    grid = "Y",
    axis_title_size = 13
    ) +
  theme(
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold")
    )

# Save it
ggsave(
  plot = preventive_basic_BB,
  here("currative-preventive", "output", "preventive_basic_BB.png"),
  dpi = 300,
  width = 2250, height = 1000, units = "px"
  )


# Add titles and description
preventive_with_titles_BB <- preventive_basic_BB +
  labs(
    x = "Years of battery life",
    y = "Battery capacity",
    title = "Preventive regeneration",
    subtitle = "Regeneration as a battery maintenance operation"
  ) #+
    # geom_text(
    #   data = labels_preventive_BB,
    #   aes(
    #     x = Year,
    #     y = Capacity,
    #     label = Capacity,
    #     ),
    #   nudge_y = 10
    #   ) +


# Save it
ggsave(
  plot = preventive_with_titles_BB,
  here("currative-preventive", "output", "preventive_with_titles_BB.png"),
  dpi = 300,
  width = 2250, height = 1000, units = "px"
)





# ******************************************************************************
# ******************************************************************************
# Currative
# ******************************************************************************
# ******************************************************************************


# Load basic data for backup batteries
df_currative_BB <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1OQSABH5vFd6GjAXIxo3FRCiZOnnETrOzJh3apjAQ8xY",
  sheet = "currative-BB",
  range = "A2:D59"
 # skip = 1
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

# Save it as csv
write_csv(df_currative_BB, here("currative-preventive", "data", "df_currative_BB.csv"))



# Create data for automatic plotting of each column
dfl_currative_BB <- pivot_longer(
  df_currative_BB,
  cols = c(2, starts_with("Reg")),
  names_to = "Regeneration",
  values_to = "Capacity"
)
# Save it as csv
write_csv(dfl_currative_BB, here("currative-preventive", "data", "dfl_currative_BB.csv"))

# Create data frame for printing labels - how much capacity battery has
labels_currative_BB <- dfl_currative_BB %>%
  mutate(
    Year = floor(Year)
  ) %>%
  select(Year, Capacity) %>%
  group_by(Year) %>%
  summarise(Capacity = max(Capacity, na.rm = TRUE ))

# Create colors scale for scale_color_manual()
colors_currative_BB <- c(
  "#0000a3", "#002800", "#004900", "#006a00", "#008a00", "#00ab00", "#00cc00", "#00ec00", "#0eff0e"
)

# Create alpha levels for scale_alpha()
alphas_currative_BB <- c("#0.9", paste("#", seq(0.7, 0.35, by=-0.05), sep = ""))

# Guide labels
guide_labels_currative_BB = c("NONE", "FIRST", "SECOND", "THIRD", "FOURTH", "FIFTH", "SIXTH", "SEVENTH", "EIGHT")


# Plot the chart
currative_basic_BB <- ggplot(
  data = dfl_currative_BB,
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
    alpha = 0.6,
    position = "dodge",
  ) +
  scale_x_continuous(
    limits = c(0, 14),
    #breaks = breaks_width(2),
    breaks = c(0, 2, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14),
    "Years of battery use"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 50, 75, 100),
    "Battery capacity [%]"
  ) +
  geom_hline(
    yintercept = 75,
    color = "red",
    size = 0.75,
    alpha = 0.75
    ) +
    geom_vline(
      xintercept = 10,
      color = "darkorange",
      size = 1,
      alpha = 0.75
      ) +
    annotate(
      "label",
      x = 10.5, y = 95,
      label = "Designed lifespan",
      size = 4,
      hjust = 0.075,
      fill = "orange",
      color = "white",
      fontface = "bold",
      alpha = 0.7
    ) +
  scale_color_manual(
    values = colors_currative_BB,
    #labels = guide_labels_currative_BB,
    guide = FALSE,
  ) +
  scale_fill_manual(
    values = colors_currative_BB,
    labels = guide_labels_currative_BB,
  ) +
  scale_alpha_manual(
    values = alphas_currative_BB,
    #guide=F
  ) +
  theme_ipsum_rc(
    grid = "Y",
    axis_title_size = 13
  ) +
  theme(
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

# Save it
ggsave(
  plot = currative_basic_BB,
  here("currative-preventive", "output", "currative_basic_BB.png"),
  dpi = 300,
  width = 2250, height = 1000, units = "px"
)


# Add titles and description
currative_with_titles_BB <- currative_basic_BB +
  labs(
    x = "Years of battery life",
    y = "Battery capacity",
    title = "currative regeneration",
    subtitle = "Regeneration as a battery maintenance operation"
  ) #+
# geom_text(
#   data = labels_currative_BB,
#   aes(
#     x = Year,
#     y = Capacity,
#     label = Capacity,
#     ),
#   nudge_y = 10
#   ) +


# Save it
ggsave(
  plot = currative_with_titles_BB,
  here("currative-preventive", "output", "currative_with_titles_BB.png"),
  dpi = 300,
  width = 2250, height = 1000, units = "px"
)

