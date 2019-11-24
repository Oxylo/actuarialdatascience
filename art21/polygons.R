#
# Polygons.R : 
#

library("tidyverse")

#
# Ex. 1 : drawing triangle & square    
# -- Note: When drawing a polygon, make sure df stores (x,y) points in drawing order!
df <- data.frame(x=c(1:3,4,5,5,4), y=c(1,4,1,1,1,4,4), 
                  shape=c(rep("triangle",3), rep("square",4)))
ggplot(data=df, mapping=aes(x=x, y=y)) +
  geom_polygon(mapping=aes(color=shape, fill=shape), alpha=0.2)

#
# Ex. 2 : Polygons below bell curve with user defined colors  
#
x <- seq(-3, 3, 0.01)
y <- 1/sqrt(2 * pi) * exp(-0.5 * x**2)
df <- data.frame(x=x, y=y)

left <-  rbind(filter(df, x < -1.96), c(-1.96,0), c(-3,0))
right <- rbind(filter(df, x> 1.96), c(3,0), c(1.96,0))
middle <- rbind( c(-1.96, 0), filter(df, abs(x)< 1.96), c(1.96,0))

ggplot(data=df, mapping=aes(x=x, y=y)) +
  geom_line(color="blue") +
  geom_polygon(data=right, fill="green", alpha=0.3) + 
  geom_polygon(data=left, fill="red", alpha=0.3) +
  geom_polygon(data=middle, fill="orange", alpha=0.3)

#
# Ex. 3: Presenting forecasts in the wonderful world of the tidyverse 
#
library("tidyverse")

df <- read.csv("/home/pieter/projects/website-ads-scripts/art21/forecasts.csv")

# Plot 1: too basic

ggplot(data=df, aes(x=year, y=expected)) +
  geom_line(aes(color=scenario)) +
  labs(title ="Plot 1: too simple")

# Plot 2: too messy

# get data in shape first!
long <- df %>% gather(key = "percentile", value="profit", c("low", "expected", "upp"))
long$line <- paste0(long$scenario, ".", long$percentile)
long$is_expected <- ifelse(long$percentile=="expected", "1", "2")
ggplot(data=long, aes(x=year, y=profit)) +
  geom_line(aes(group=line, color=scenario, linetype=is_expected)) +
  labs(title ="Plot 2: too messy") +
  guides(linetype=FALSE)

# Plot 3: Nice! 

filtered.upp <- long %>%
  filter(percentile == "upp")
filtered.low <- long %>%
  filter(percentile == "low") %>% 
  arrange(desc(year))
df2 <- rbind(filtered.upp, filtered.low)

ggplot(data=long, aes(x=year, y=profit)) +
  geom_line(aes(group=line, color=scenario, linetype=is_expected)) +
  geom_polygon(data=df2, aes(fill=scenario), alpha=0.2) +
  labs(title ="Plot 3: nice!") +
  guides(linetype=FALSE)

