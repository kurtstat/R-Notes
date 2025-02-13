# the geom_tile() script

# Step 0 - install/load packages
library(tidyverse)
library(cowplot)

#Step 1 - create a dataframe

# Creating vectors
x <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5)
y <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 3, 4)
value <- c(3, 6, 12, 18, 24, 25, 27, 30, 30, 30, 31, 27, 34, 12, 16, 0.7, 72, 34)
# Creating a dataframe
df <- data.frame(x = x,
                 y = y,
                 value = value)

# Step 2 draw a plot

plot_ward_5 <-
  ggplot(data = df) +
  aes(x = x, 
      y = y,
      fill = value) +
  geom_tile(color = "white",
            lwd = 1,
            linetype = 1) + 
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Ward 5 (18 beds)") +
  theme(plot.title = element_text(hjust = 0.08,
                                  vjust = -1))

# Create a second ward etc.
# Creating vectors
x1 <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5)
y1 <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
value1 <- c(3, 6, 12, 18, 24, 25, 27, 30, 30, 30, 31, 27, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
# Creating a dataframe
df1 <- data.frame(x = x1,
                 y = y1,
                 value = value1)

# Step 2 draw a plot

plot_ward_6 <-
  ggplot(data = df1) +
  aes(x = x1, 
      y = y1,
      fill = value1) +
  geom_tile(color = "white",
            lwd = 1,
            linetype = 1) + 
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Ward 6 (20 beds)") +
  theme(plot.title = element_text(hjust = 0.08,
                                  vjust = -1))

plot_grid(plot_ward_5,
          plot_ward_6,
          rel_heights = c(1,1), 
          ncol = 2)

