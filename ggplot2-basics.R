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

head(df[, c("wt", "mpg", "cyl")], 3)

