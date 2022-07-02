
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


# Load basic data
df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1OQSABH5vFd6GjAXIxo3FRCiZOnnETrOzJh3apjAQ8xY",
  sheet = "preventive",
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

# Save it as csv
write_csv(df, here("currative-preventive", "data", "df.csv"))


# Plot the basic df
# Each column has own geom_area
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
  theme_ipsum(grid = "Y")




# Create data for automatic plotting of each column
dfl <- pivot_longer(
  df,
  cols = c(2, starts_with("Reg")),
  names_to = "Regeneration",
  values_to = "Capacity"
)
# Save it as csv
write_csv(dfl, here("currative-preventive", "data", "dfl.csv"))

# Create data frame for printing labels - how much capacity battery has
labels <- dfl %>%
  mutate(
    Year = floor(Year)
  ) %>%
  select(Year, Capacity) %>%
  group_by(Year) %>%
  summarise(Capacity = max(Capacity, na.rm = TRUE ))

# Create colors scale for scale_color_manual()
colors <- c(
  "#0000a3", "#002800", "#004900", "#006a00", "#008a00", "#00ab00", "#00cc00", "#00ec00", "#0eff0e"
  )

# Create alpha levels for scale_alpha()
alphas <- c("#0.9", paste("#", seq(0.7, 0.35, by=-0.05), sep = ""))

# Guide labels
guide_labels = c("WITHOUT", "FIRST", "SECOND", "THIRD", "FOURTH", "FIFTH", "SIXTH", "SEVENTH", "EIGHT")


# Plot the chart
preventive_basic <- ggplot(
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
  scale_color_manual(
    values = colors,
    #labels = guide_labels,
    guide = FALSE,
  ) +
  scale_fill_manual(
    values = colors,
    labels = guide_labels,
  ) +
  scale_alpha_manual(
    values = alphas,
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
  plot = preventive_basic,
  here("currative-preventive", "output", "preventive_basic.png"),
  dpi = 300,
  width = 2250, height = 1000, units = "px"
  )


# Add titles and description
preventive_with_titles <- preventive_basic +
  labs(
    x = "Years of battery life",
    y = "Battery capacity",
    title = "Preventive regeneration",
    subtitle = "Regeneration as a battery maintenance operation"
  ) #+
    # geom_text(
    #   data = labels,
    #   aes(
    #     x = Year,
    #     y = Capacity,
    #     label = Capacity,
    #     ),
    #   nudge_y = 10
    #   ) +


# Save it
ggsave(
  plot = preventive_with_titles,
  here("currative-preventive", "output", "preventive_with_titles.png"),
  dpi = 300,
  width = 2250, height = 1000, units = "px"
)





# ******************************************************************************
# Currative
# ******************************************************************************






