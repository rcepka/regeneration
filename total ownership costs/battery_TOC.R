
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  readxl,
  highcharter,
  here,
  directlabels,
  ggthemes,
  scales,
  ggforce
)


# Import data from excell
# ...for backup batteries
data_backup <- read_excel(
  here("data", "battery_total_ownership_costs.xlsx"),
  sheet = 1,
  skip = 21
  )

# ...for forklift batteries
data_forklift <- read_excel(
  here("data", "battery_total_ownership_costs.xlsx"),
  sheet = 2,
  skip = 21
  )



# tidy and prepare the data

data_backup <- data_backup %>%
  # select(
  #   -14,
  #   -15
  # ) %>%
  rename(
    Years.Initial = 1,
    Years.Prolonged = 2,
    Life.Total = 3,
    No.Reg.TOC = 4,
    No.Reg.OC.Yearly = 5,
    Regeneration.Costs = 6,
    Savings.Lifetime = 13
  ) %>%
  mutate(
    Years.Prolonged = as.character(Years.Prolonged),
    Regeneration.Costs = Regeneration.Costs * 100,
    #Regeneration.Costs = paste(Regeneration.Costs * 100, "%", sep = "")
  ) %>%
  write_csv(here("data", "battery_TOC_backup.csv"))


ggplot(data_backup,
       aes(x = Years.Initial, y =  Savings.Lifetime)
       ) +
  geom_point(
    aes(
      color = Years.Prolonged, # color
      size = Life.Total # size
      )
    ) +
  geom_line(
    aes(
      group = Years.Prolonged,
      #group = Regeneration.Costs,
      color = Years.Prolonged
      )
    ) +
  geom_mark_ellipse(
    aes(
     # fill = Savings.Lifetime,
      filter = (Years.Initial == 7 & Years.Prolonged == 3) | (filter = Years.Initial == 7 & Years.Prolonged == 4),
      label = "Expected alternative"
      ),
    label.fontsize = 8,
    color = "orange",
    fill = "orange",
    alpha = 0.75,
    expand = unit(3, "mm")
  ) +
  scale_x_continuous(
    limits = c(5, NA)
  ) +
  geom_text(
    aes(
      label = Years.Prolonged,
      x = Years.Initial + 0.40
      ),
    # color = "white",
    # fill = "orange",
    fontface = "bold",
    data = data_backup %>% filter(Years.Initial == max(Years.Initial)),
    #nudge_x = 0.40,
    size = 3
  ) +
  facet_wrap(
    ~Regeneration.Costs,
    ncol = 2,
    scales = "free_y"
   ) +
  theme_light() +
  labs(
   # x = "sdsd",
  #  y = "fddfdfd"
  )

  ggsave(
    here("output", "backup_batteries.png"),
    # width = 2000,
    # height = 2500,
    # units = c("px")
  )






ggplot(data_backup,
       aes(x = Years.Prolonged, y =  Savings.Lifetime, group = Regeneration.Costs)
) +
  geom_point(
    aes(
      color = Years.Initial, # color
      size = Life.Total # size
    )
  ) +
  # geom_mark_ellipse(
  #   aes(
  #     # fill = Savings.Lifetime,
  #     filter = (Years.Initial == 7 & Years.Prolonged == 3) | (filter = Years.Initial == 7 & Years.Prolonged == 4),
  #     label = "Expected alternative"
  #   ),
  #   label.fontsize = 8,
  #   color = "orange",
  #   fill = "orange",
  #   alpha = 0.75,
  #   expand = unit(3, "mm")
  # ) +
  # scale_x_continuous(
  #   limits = c(5, NA)
  # ) +
  geom_text(
    aes(
      label = Years.Prolonged,
      x = Years.Initial + 0.40
    ),
    # color = "white",
    # fill = "orange",
    fontface = "bold",
    data = data_backup %>% filter(Years.Initial == max(Years.Initial)),
    #nudge_x = 0.40,
    size = 3
  ) +
  # facet_wrap(
  #   ~Regeneration.Costs,
  #   ncol = 2,
  #   scales = "free_y"
  # ) +
  theme_light() +
  labs(
    # x = "sdsd",
    #  y = "fddfdfd"
  ) +
  geom_jitter()













#geom_dl(aes(label = Years.Prolonged), method = "last.points", cex = 0.8) +
  geom_text(
    aes(
      x = 10,
      y = 100
      ),
    label = "Yers\n prolonged",
   # data = data_backup %>% filter(Years.Initial == max(Years.Initial) & Savings.Lifetime == max(Savings.Lifetime)),
    size = 3
    )


  annotate(
    "text",
    x = 10,
    y = 180,
    label = "Added years",
    size = 3,
    #color = "orange"
    )

  # geom_smooth(
  #   method = "lm",
  #   size = 2,
  #   se = FALSE
  #  ) #+
  stat_ellipse(
    data = data_backup %>%
      filter(
        Years.Initial == 7,
        Years.Prolonged == 3
        )
    )
  #geom_smooth(stat = "identity")



test <-  data_backup %>%
  filter(
    Years.Initial == 7,
    Years.Prolonged == 3
  )

ggplot(data = test) +
  stat_ellipse(
    aes(
      x = Years.Initial,
      y =  Savings.Lifetime,
      group = Savings.Lifetime
      )
  )






path <- read_excel(
  here("data", "battery_total_ownership_costs.xlsx")
) %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)

sheets <- excel_sheets(here("data", "battery_total_ownership_costs.xlsx"))
sheets
