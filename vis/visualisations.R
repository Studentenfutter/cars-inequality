# Plots and visualizations

# Avg. car price per group model | classes_region from Grouping.R
library(ggplot2)
library(hrbrthemes)
ggplot(ebay_grouped) +
  aes(x = model, y = avg.price) +
  geom_boxplot(fill = "#6baed6") +
  labs(x = "Model Groups", y = "Price in €", title = "Average car price per model group") +
  coord_flip() +
  theme_ipsum() 

# Plots from Alexander
library(dotwhisker)
library(ggpmisc)
load("data/other_data/plz_codes.Rda")

# eb <- ebay_clean
# eb2 <- ebay_clean %>% filter(price < 123456)
# eb$age <- 2019 - as.numeric(eb$registration_date) # Calcuate age of car
# eb <- eb %>% filter(age < 100)
# eb2 <- eb %>%  distinct(text, .keep_all= TRUE)
# eb2$kilometer <- eb2$kilometer %>% as.numeric()
# eb2 <- eb2 %>% filter(kilometer > 500)
# 
# load("data/other_data/external.data2.Rda")
# eb3 <- left_join(eb2, ext3, by = "RegionalID")
# eb3$gdppc.2017 <- eb3$gdppc.2017 %>% as.numeric()
# 
# theft <- read.csv("data/other_data/theft.csv")
# theft$Gemeinde.schlÃ.sselneu <- paste0("0", stringr::str_extract(theft$Gemeinde.schlÃ.ssel, "(\\d{4})"))
# eb4 <- left_join(eb3, theft, by = c("RegionalID" = "Gemeinde.schlÃ.sselneu"))
eb4 <- read.csv("data/other_data/eb4.csv")

formula <- y~x

# Replaced Place with zip_extracted
e1<- ggplot(eb4, aes(kilometer, price, group = Place, color = Place))+
  geom_point(size = 3, alpha = 0.4)+ theme_minimal()+ scale_color_viridis_d()+
  facet_wrap(~Place, nrow = 3)+
  geom_smooth(method = "lm", se = F)+ ylim(0, 40000)+ xlim(0, 200000)
 + stat_poly_eq(aes(label = ..eq.label..), vjust = c(-8, 0), formula = formula, parse = TRUE)

m1 <- lm(price ~ age + kilometer+  Place+ gdppc.2017+ HZ.nach.Zensus, data = eb4)

p <- dwplot(m1, dodge_size = 0.2, dot_args = list(size = 2), whisker_args = list(size = 1)) +
  xlab("Price changes in €") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  labs(title = "Regression on effects on price with Deggendorf as a reference category") +
  #facet_wrap(~model, ncol = 1, scales = "free_y") +
  theme_bw() + scale_color_grey(start = 0, end = 0.6) +
  #labs(color = "MOOC") +
  theme(legend.position = "none")

