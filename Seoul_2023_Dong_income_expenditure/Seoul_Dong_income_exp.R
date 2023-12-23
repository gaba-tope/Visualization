## Import library---------------------------
library(tidyverse)
library(plyr)
library(showtext)
library(ggtext)
library(ggrepel)
library(ggsflabel) # devtools::install_github("yutannihilation/ggsflabel")
library(sf) # import and use .shp file
library(shiny)
library(ggiraph) # Interactive plot
library(scales)
options(scipen = 999) # Disable scientific notation.

script_path <- dirname(rstudioapi::getSourceEditorContext()$path) # Path of current script file
setwd(script_path)
## Fonts---------------------------------------------------
font_add_google(name = "Nanum Gothic", family = "nanum")
font_add_google(name = "Noto Sans KR", family = "notosans")
font_add_google(name = "Roboto", family = "roboto")
sysfonts::font_add(family = "Font Awesome 6 Brands", # Social media icon fonts
                   regular = "C:/R/Rwd/fonts/Font-Awesome-6-Brands-Regular-400.otf")
showtext_auto()

main_font <- "notosans"
num_font <- "roboto"


## Colors -------------------------------------------------
bg_col <- "#eeeeee"
text_col <- "grey10"
lighter_text_col <- "#7d7d7d"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"

## Data Import---------------------------------------------
raw_seoul <- read.csv("./Seoul_income_expendit_dong.csv",fileEncoding = "euc-kr") #https://data.seoul.go.kr/dataList/OA-22168/S/1/datasetView.do
head(raw_seoul)

## Data Wrangling -----------------------------------------
# Seoul Income Expenditure per Dong Dataset
seoul <- raw_seoul
colnames(seoul) <- c("quarter", "dong_code", "dong_name", "mean_income", "income_interval",
                         "sum_e", "grocery_e", "clothes_e", "life_e", "medical_e",
                         "transport_e", "edu_e", "play_e", "hobby_e", "etc_e", "food_e")
seoul_20231 <- dplyr::filter(seoul, quarter == 20231)
seoul_20231 <- as_tibble(seoul_20231)
seoul_20231$EMD_CD <- as.character(paste0(seoul_20231$dong_code, "00"))

head(seoul_20231)
dim(seoul_20231)
# Raw data is outdated than map data. Updated the raw data: EMD_CD (17th col) and dong_name (3rd col)
seoul_20231_updated <- seoul_20231
seoul_20231_updated[seoul_20231_updated$dong_code == 11680740, 17] <- "1168067500" # 일원2동 -> 개포3동 renamed
seoul_20231_updated[seoul_20231_updated$dong_code == 11680740, 3] <-  "개포3동" # 일원2동 -> 개포3동 renamed
seoul_20231_updated[seoul_20231_updated$dong_code == 11740520, 17] <- "1174052500" # 상일동 -> 상일제1동 renamed
seoul_20231_updated[seoul_20231_updated$dong_code == 11740520, 3] <- "상일제1동" # 상일동 -> 상일제1동 renamed
seoul_20231_updated[nrow(seoul_20231_updated)+1, ] <- NA # New empty row added
seoul_20231_updated[nrow(seoul_20231_updated), 1:3] <- list(20231, 11740526,"상일제2동") # 상일제2동 row added (South region of 강일동 became 상일제2동)
seoul_20231_updated[nrow(seoul_20231_updated), 4:16] <- seoul_20231_updated[283, 4:16] # 상일제2동 data is the same as 강일동. 
seoul_20231_updated[nrow(seoul_20231_updated), 17] <- "1174052600"


# Seoul Dong polygon Data
map_seoul <- st_read("./TL_SCCO_GEMD.shp", options = "ENCODING=euc-kr") # Data last updated in 2022-11
EMD_CD_coded <- ddply(map_seoul, .(EMD_CD), function(row){
                          row$dong_code <- (gsub('.{2}$', "", row$EMD_CD))
                          })
# Combine sf and data
combined_data <- left_join(map_seoul, seoul_20231_updated, by = "EMD_CD") # The order of x and y matters.
combined_data |> filter(is.na(dong_code)) 

# Seoul Gu polygon Data
map_seoul_gu <- st_read("./TL_SCCO_SIG.shp", options = "ENCODING=euc-kr")# Map at 2022-11

## Texts --------------------------------------------------
# Social Info
github_icon <- "&#xf09b" #unicode f09b, used for HTML so &#x added.
github_username <- "gaba-tope"
twit_icon <- "&#xf099"
twit_username <- "@tope_ezia"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #E30B5C; font-family:\"roboto\";'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{twit_icon};</span>
  <span style='color: #E30B5C; font-family:\"roboto\";'>{twit_username}</span>"
)
# Map Plot Text
title_map <- "2023년 서울특별시 행정동별 평균 소득"
data_info <- "서울특별시 상권분석서비스 (소득소비) (2023-11-13)"
cap_map <- glue::glue(
  "<span style='font-family:\"roboto\";'>**Data**</span>: {data_info}<br>
  <span style='font-family:\"roboto\";'>**Graphic**: Tope </span>{social_caption}")
legend_map <- "평균 소득 (원)"

## Themes -------------------------------------------------
# Map Theme
map_theme <- theme(
  plot.title.position = "plot", 
  plot.caption.position = "plot",
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = main_font,
    lineheight = 0.5,
    size = 40,
    margin = margin(b = 2, t = 2) 
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    family = main_font,
    size = 30,
    margin = margin(b = 10)
  ),
  plot.caption = element_textbox_simple(
    colour= text_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5),
    size = 30
  ),
  axis.text.x = element_blank(), # No axis text and title.
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  legend.text = element_text(colour = text_col,
                             family = num_font,
                             size = 25,
                             margin = margin(l = -5, r = -10)
  ),#
  legend.title = element_text(colour = text_col,
                              family = main_font,
                              size = 30,
                              lineheight = 0.5,
                              face = "bold",
                              margin = margin(b= -10)
  ), #face = "bold"
  legend.background = element_rect(fill = bg_col)
)
# Theme for interactive Map
map_int_theme <- theme(
  plot.title.position = "plot", 
  plot.caption.position = "plot",
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = main_font,
    lineheight = 0.5,
    size = 40,
    margin = margin(b = 2, t = 2) 
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    family = main_font,
    size = 30,
    margin = margin(b = 10)
  ),
  plot.caption = element_textbox_simple(
    colour= text_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5),
    size = 30
  ),
  axis.text.x = element_blank(), # No axis text and title.
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  legend.text = element_text(colour = text_col,
                             family = num_font,
                             size = 25,
                             margin = margin(l = -5)
  ),#
  legend.title = element_text(colour = text_col,
                              family = main_font,
                              size = 30,
                              lineheight = 0.5,
                              face = "bold",
                              margin = margin(b = -10)
  ), #face = "bold"
  legend.background = element_rect(fill = bg_col),
  legend.key.width = unit(1.5,"cm"),
  legend.key.height = unit(2,"cm")
  
)



## Plots --------------------------------------------------
# Designed Map w/ {ggplot2}
seoul_plot <-   ggplot()+
  geom_sf(data = combined_data, aes(fill = mean_income), color = major_grid_col, linewidth = 0.1)+
  geom_sf(data = map_seoul_gu, color = text_col, alpha = 0 )+
  scale_fill_distiller(palette = "Purples", direction = 1, labels = scales::label_comma(),
                       limits = c(2000000, 7500000), breaks = seq(2000000, 7500000, 1000000))+
  geom_sf_text(data = map_seoul_gu,
                aes(label = ifelse(!(SIG_KOR_NM %in% c("양천구", "강남구", "동작구", "성북구")), SIG_KOR_NM, "")), 
                colour = "black")+ # "양천구" text overlaps with the border. "강남구" text is on dark bg.
                                       #  "동작구" and "성북구" not centered. 
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "양천구", SIG_KOR_NM, "")), 
                     colour = "black", nudge_x = -10, nudge_y = -10 # 양천구
                     )+
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "강남구",  SIG_KOR_NM, "")),
                     colour = "black", nudge_x = -30 , nudge_y = +20
                     )+ # 강남구
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "동작구",  SIG_KOR_NM, "")),
                     colour = "black", nudge_x = 0 , nudge_y = +30
                    )+ # 동작구
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "성북구",  SIG_KOR_NM, "")),
                     colour = "black", nudge_x = -10 , nudge_y = -20
                    )+ # 성북구
  labs(title = title_map,
     caption = cap_map,
    fill = legend_map)+
  map_theme

ggsave(file="seoul_plot.png", plot = seoul_plot, width = 2000, height= 1400, 
       units = 'px', dpi = 300)

# Interactive Map with {ggiraph}
seoul_plot_int <-   ggplot()+
  geom_sf_interactive(data = combined_data, aes(fill = mean_income),
                      tooltip =paste(combined_data$EMD_KOR_NM, ":", comma(combined_data$mean_income),"원"),
                      color = lighter_text_col, linewidth = 0.5)+ 
  geom_sf(data = map_seoul_gu, color = text_col, alpha = 0, linewidth = 1 )+
  scale_fill_distiller(palette = "Purples", direction = 1, labels = scales::label_comma(),
                       limits = c(2000000, 7500000), breaks = seq(2000000, 7500000, 1000000))+
  geom_sf_text(data = map_seoul_gu,
               aes(label = ifelse(!(SIG_KOR_NM %in% c("양천구", "강남구", "동작구", "성북구")), SIG_KOR_NM, "")), 
               colour = "black", size = 6)+ # "양천구" text overlaps with the border. "강남구" text is on dark bg.
  #  "동작구" and "성북구" not centered. 
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "양천구", SIG_KOR_NM, "")), 
                     colour = "black", nudge_x = -10, nudge_y = -10, size = 6 # 양천구
  )+
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "강남구",  SIG_KOR_NM, "")),
                     colour = "black", nudge_x = -40 , nudge_y = +30, size = 6
  )+ # 강남구
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "동작구",  SIG_KOR_NM, "")),
                     colour = "black", nudge_x = 0 , nudge_y = +30, size = 6
  )+ # 동작구
  geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "성북구",  SIG_KOR_NM, "")),
                     colour = "black", nudge_x = -10 , nudge_y = -20, size = 6
  )+ # 성북구
  labs(title = title_map,
       caption = cap_map,
       fill = legend_map)+
  map_int_theme

tooltip_css <- "background-color:#d8118c;color:white;padding:5px;border-radius:3px;"

girafe_map_int <- girafe(ggobj = seoul_plot_int,
       width_svg = 35, height_svg = 17) |> girafe_options(opts_hover(css = "fill:yellow;stroke:black;stroke-width:3px;"),
                                                 opts_tooltip(css = tooltip_css, opacity = 1),
                                                 opts_zoom(min = .7, max = 2)) 
htmlwidgets::saveWidget(girafe_map_int, file="girafe_map_int.html")

## Shiny (For Practice) -------------------------------------------
plotdf <- data.frame()

'''
Tried to make a function that makes an appropriate breaks.. but failed.
breaksetter <- function(limits, breaks_num){ # limits:a vector that specify limits. breaks_num: number of breaks.
  rounder <- function(number) { # Round number to the nearest order of magnitude (e.g. 248,000 -> 200,000)
    order_magni <- log10(number) |> floor()
    base <- 10^order_magni
    rounded_num <- round(number/base) * base
    return(rounded_num)
  }
  rounded_limits <- rounder(limits) # e.g. c(200000, 890000) -> c(200000, 900000)
  break_val <- ((rounded_limits[2] - rounded_limits[1]) / breaks_num) |> rounder() 
  rounded_min <- limits[1] |> rounder()
  rounded_max <- limits[2] |> rounder()
  result <- seq(rounded_min, rounded_max, break_val) 
  return(result)
}
'''

Map_var <- function(df = combined_data, data_type){
  data_type <- sym(data_type) #https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot was helpful
  
  plotdf <- df |> dplyr::select(EMD_CD, dong_name, !!data_type, geometry)
  data_type_char <- ifelse(names(plotdf)[3] == "mean_income", "평균 소득", " ")
  title_map_s <- glue::glue("2023년 서울특별시 행정동별 {data_type_char}")
  cap_map_s <- cap_map
  legend_map_s <- glue::glue("{data_type_char} (원)")
  legend_limits <- c(min(df[[data_type]]), max(df[[data_type]]))
  legend_breaks <- seq(2000000, 7500000, 1000000)
    
  g <- ggplot()+
    geom_sf_interactive(data = df, aes(fill = !!data_type),
                        tooltip = paste(df$dong_name, ":", comma(df[[data_type]]), "원"), # glue::glue("{dong_name}: {data_type}"),
                        color = major_grid_col, linewidth = 0.1)+
    geom_sf(data = map_seoul_gu, color = text_col, alpha = 0.1, linewidth = 1)+
    scale_fill_distiller(palette = "Purples", direction = 1, labels = scales::label_comma(),
                         limits = legend_limits, breaks = legend_breaks)+
    geom_sf_text(data = map_seoul_gu,
                 aes(label = ifelse(!(SIG_KOR_NM %in% c("양천구", "강남구", "동작구", "성북구")), SIG_KOR_NM, "")), 
                 colour = "black", size = 6)+ # "양천구" text overlaps with the border. "강남구" text is on dark bg.
    #  "동작구" and "성북구" not centered. 
    geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "양천구", SIG_KOR_NM, "")), 
                       colour = "black", nudge_x = -10, nudge_y = -10, size = 6 # 양천구
    )+
    geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "강남구",  SIG_KOR_NM, "")),
                       colour = "black", nudge_x = -30 , nudge_y = +20, size = 6
    )+ # 강남구
    geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "동작구",  SIG_KOR_NM, "")),
                       colour = "black", nudge_x = 0 , nudge_y = +30, size = 6
    )+ # 동작구
    geom_sf_text_repel(data = map_seoul_gu, aes(label = ifelse(SIG_KOR_NM == "성북구",  SIG_KOR_NM, "")),
                       colour = "black", nudge_x = -10 , nudge_y = -20, size = 6
    )+ # 성북구
    labs(title = title_map_s,
         caption = cap_map_s,
         fill = legend_map_s)+
    map_int_theme
    
                        
  return(g)
}


  ui <- fluidPage(
  #theme <- bslib::bs_theme(
   # bg = bg_col, 
    #fg = "white", 
    #base_font = main_font
  #),
  titlePanel("서울시 행정동별 소비 및 지출 데이터셋"),
  sidebarLayout(
    column(width = 8,
        sidebarPanel(
          selectInput(inputId = "data_type",
                  label = "보고자하는 데이터를 선택하세요:",
                  choices = list("평균 소득" = "mean_income")
          ), width = 4
        )
      ),
    column(width = 12, offset = 1,
        mainPanel(girafeOutput("main_plot", width = "1600px", height = "1200px")
        )
    )
  )
)

      

server = function(input, output){
  tooltip_css <- "background-color:#d8118c;color:white;padding:5px;border-radius:3px;"
  output$main_plot <- renderGirafe({
    girafe(code = print(Map_var(combined_data, data_type = input$data_type)),
           width_svg = 30, height_svg = 30,
           ) |> girafe_options(opts_hover(css = "fill:yellow;stroke:black;stroke-width:3px;"),
                                                              opts_tooltip(css = tooltip_css, opacity = 1)) 
                                  })
}


shinyApp(ui = ui, server = server, options = list(height = 600))
