
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  googlesheets4,
  highcharter,
  here,
  directlabels,
  ggthemes,
  scales,
  ggforce
)

df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1OQSABH5vFd6GjAXIxo3FRCiZOnnETrOzJh3apjAQ8xY",
  skip = 1
  ) %>%
  rename(
    No.Regeneration = 2,
    Regeneration1 = 3,

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
    )
  )


  #   #Year = as.character(Year),
  #   Year = str_replace(Year, ",", "."),
  #   Year = as.numeric(Year),
  #   No.Regeneration = str_replace(No.Regeneration, ",", "."),
  #   No.Regeneration = str_replace(No.Regeneration, "%", ""),
  #   No.Regeneration = as.numeric(No.Regeneration)
  #
  # )


df_longer

ggplot(
  data = df,
  aes(x = Year, y = No.Regeneration)
  ) +
  geom_area(
      alpha = 0.1,
      color = "red",
      fill = "red"
      )





ggplot(
  data = df,
  aes(x = Year)
) +
  geom_area(
    aes(
      y = No.Regeneration
      ),
    alpha = 0.1,
    color = "red",
    fill = "red"
  ) +
  geom_area(
    aes(
      y = Regeneration1
      ),
      color = "blue",
      fill = "blue",
      alpha = 0.2,
    )






df <- data.frame(
  Years = (1:15),
  Capacity = c(1,
              0.95,
              0.925,
              0.9,
              0.85,
              0.825,
              0.775,
              0.75,
              0.725,
              0.675,
              0.60,
              0.50,
              0.4,
              0.3,
              0.20),
  Regeneration.1 = c(NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     0.95,
                     0.9,
                     0.85,
                     0.80,
                     0.7,
                     0.6,
                     0.5,
                     0.4,
                     0.3),
  Regeneration.2 = c(NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     0.90,
                     0.85,
                     0.80,
                     0.75,
                     0.6,
                     0.5,
                     0.4)
)


df2 <- data.frame(
  Y = rep(1:10, each = 4)
)



df2
df <- as_tibble(df)

df <- df %>%
  mutate(
    #Capacity = as.numeric(paste(Capacity * 100, "%", sep = ""))
    Capacity = Capacity * 100,
    Regeneration.1 = Regeneration.1 * 100
  )


ggplot(data = df,
       aes(Years)
       ) +
  geom_area(
    aes(y = Capacity
        ),
    color = "blue",
    fill = "blue",
    alpha = 0.2) +
  geom_area(
    aes(y = Regeneration.1
    ),
    color = "green",
    fill = "green",
    alpha = 0.7) +
  geom_area(
    aes(y = Regeneration.2
    ),
    color = "red",
    fill = "red",
    alpha = 0.85) +
  scale_x_continuous(
    breaks = c(1:15)
  ) +
  annotate(
    "segment",
    x = 1,
    y = 75,
    xend = 15,
    yend = 75,
    color = "red",
    size = 1

  )




df
df <- data.frame(
  Years = (1:15)
)
df
