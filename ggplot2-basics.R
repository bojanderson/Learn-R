library(ggplot2)

# Box Plot

# Basic box plot
p <- ggplot(ToothGrowth, aes(x="", y=len)) +
  geom_boxplot(fill="slateblue", notch=T)

p

# Rotate the box plot
p + coord_flip() + theme_classic()

# Donut Chart

library(dplyr)

# Create Data Frame
count.data <- data.frame(
  class = c("Pear", "Orange", "Grapefruit", "Bananas"),
  n = c(325, 285, 706, 885),
  prop = c(14.8, 12.9, 32.1, 40.2)
)

count.data

# Manipulate data...?
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

count.data

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

ggplot(count.data, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(y = lab.ypos, label = prop), color = "white") +
  scale_fill_manual(values = mycols) +
  theme_void() +
  xlim(0.2, 2.5)

# Line Chart

USPersonalExpenditure

us <- as.data.frame(t(USPersonalExpenditure))

us$year <- rownames(us2)

ggplot(data = us, aes(x=year, y=`Food and Tobacco`, group = 1)) +
  geom_line(color="gold") +
  geom_point(color = "navy") +
  theme_light()
  
# Scatter Plot

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point()

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size = 4, shape = 20)

# Multiple Box Plot

ToothGrowth$dose = as.factor(ToothGrowth$dose)

ggplot(ToothGrowth, aes(x = dose, y = len, color = dose)) +
  geom_boxplot(notch = T) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  theme_classic()

ggplot(ToothGrowth, aes(x = dose, y = len, group = dose, color = dose, fill = dose)) +
  geom_boxplot(color = 'black') +
  theme_classic() + 
  coord_flip()

# Bar Chart

library(datasets)

ggplot(chickwts, aes(x = feed, y = weight, fill = feed)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  coord_flip() +
  theme(legend.position = "none")

# Bubble Chart

theme_set(theme_bw() + theme(legend.position = "top"))

# Load Data

df <- mtcars

df$cyl <- as.factor(df$cyl)

ggplot(df, aes(x = wt, y = mpg)) +
  geom_point(aes(color = cyl)) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12))

# Heat Map

nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

head(nba)

nba$Name <- with(nba, reorder(Name, PTS))

head(nba)

library(reshape2)
library(plyr)
library(scales)

nba.m <- melt(nba)

nba.m <- ddply(nba.m, .(variable), transform, rescale = rescale(value))

ggplot(nba.m, aes(variable, Name)) +
  geom_tile(aes(fill = rescale), colour = "white") +
  scale_fill_distiller(palette = "YlOrRd") +
  guides(fill = guide_legend(title = "Rating"))

# Lollipop Chart

library(dplyr)

ohio <- midwest %>%
  filter(state == "OH") %>%
  select(county, percollege) %>%
  arrange(percollege) %>%
  mutate(Avg = mean(percollege, na.rm = T),
         Above = ifelse(percollege - Avg > 0, T, F),
         county = factor(county, levels = .$county))

ggplot(ohio, aes(percollege, county, color = Above)) +
  geom_segment(aes(x = Avg, y = county, xend = percollege, yend = county),
               color = "grey50") +
  geom_point() +
  theme(text = element_text(size = 7))

# Multi-Bar Chart

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

ggplot(data = ToothGrowth, aes(fill = supp, x = dose, y = len)) +
  geom_bar(position = "dodge", stat = "identity")

# Facet Grid Chart

sp <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(shape = 2)

sp

sp + facet_grid(vs ~ cyl)
sp + facet_wrap( ~ cyl)

# Scatterplot Matrix

library(GGally)

mtcars$am <- as.factor(mtcars$am)

ggpairs(mtcars, columns = 1:4, title = "MT Cars", mapping = ggplot2::aes(colour = am), axisLabels = "show")

# Chloropeth Map

library(stringr)
library(maps)

states <- as.data.frame(state.x77)
states$region <- tolower(rownames(states))

states_map <- map_data("state")
fact_join <- left_join(states_map, states, by = "region")

ggplot(fact_join, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Murder), color = "white") +
  scale_fill_viridis_c(option = "C")
