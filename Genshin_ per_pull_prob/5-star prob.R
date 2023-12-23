# 5-star_trial-probability plot

# Load Packages -------------
library(ggplot2)
library(ggtext)
library(showtext)

script_path <- dirname(rstudioapi::getSourceEditorContext()$path) # Path of current script file
setwd(script_path)

# Load Data -------------
# Data from "https://genshin.gamedot.org/?mid=board&target=view&board=tip&post=290"
raw <- read.csv("./5-star prob.csv", header = F)  # Relevant File Path


# Load Fonts -------------
font_add_google("Gowun Dodum", "Gowun Dodum")
showtext_auto()

# Data Wrangling -------------
colnames(raw) <- c('trial', 'probability')

# Define colors -------------
line_col <- "lightgrey"
point_fill <- "#707070"
point_col <- "black"
bg_fill <- "white"
bg_col <- "black"
grid_col <- "#ebebeb"
ann1_col <- "skyblue"
ann2_col <- "lightgreen"

# Texts
 title_gen <- "**기원 횟수 당 5성 출현 확률 (per-pull probability)**"
 lab_x <- "기원 횟수 (번)"
 lab_y <- "출현 확률 (%)"
# Plot
ggplot(raw, aes(x = trial, y = probability)) +
  geom_line(color = line_col, linewidth = 2)+  # Line graph
  geom_point(shape = 21, color = point_col, fill = point_fill, size = 1.5) + # Point
  scale_x_continuous(breaks = seq(0, 100, by = 5)) + # x axis tick interval
  scale_y_continuous(breaks = seq(0, 110, by = 10)) + # y axis tick interval
  labs(title = title_gen,
       x = lab_x  ,
       y = lab_y) +
  theme(text=element_text(size=16,  family="Gowun Dodum"),
        #plot.title = element_text(hjust = 0.5 ,size=20,family='Gowun Dodum'),
        panel.background = element_rect(fill= bg_fill, colour=bg_col, size=1),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = grid_col),
        plot.title = element_textbox_simple(
          colour= "black",
          face = "bold",
          family = "Gowun Dodum",
          lineheight = 0.5,
          size = 20, # for png, size = 35. svg: 25
          margin = margin(b = 5, t = 5, r = 5) # plot title 기본 위치를 기준으로 변경.
        ))+
  annotate("rect", xmin = 73, xmax = 91, ymin = 5.0, ymax = 101,
           alpha = .2, fill=ann1_col) +
  annotate("text", x = 80, y = 90, label = "확률 증가 구간", family="Gowun Dodum") +
  annotate("text", x = 77, y = 86, label = "(74회부터 회당 6%p 증가)", family="Gowun Dodum") +
  annotate("rect", xmin = 0, xmax = 74, ymin = -1, ymax = 2,
           alpha = .2, fill=ann2_col) +
  annotate("text", x= 6, y = 4, label = "1회~73회까지는 0.6% 고정 확률", family = "Gowun Dodum")
