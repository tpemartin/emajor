## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=F------------------------------------------------------------------
#  remotes::install_github("tpemartin/emajor")

## ----setup--------------------------------------------------------------------
library(emajor)

library(dplyr); library(stringr); library(ggplot2); library(plotly); library(lubridate); library(readr); library(tidyr); library(showtext);library(grid);library(readxl);library(gridExtra); library(reshape2)

font_add_google("Noto Serif TC", "NSerifC") #google字體
showtext_auto(enable=TRUE) #啟用字體
data("indicator")

## ----message=FALSE, warning=FALSE---------------------------------------------
indicator %>% ggplot(aes(x = 年月)) +
  geom_hline(yintercept = 100, color = "black", linetype = "longdash", size = 0.3) + # 標準線
  geom_line(aes(y = 領先指標), color = "#1f78b4", size = 1.5) + # geom
  geom_line(aes(y = 同時指標), color = "#33a02c", size = 1.5) +
  geom_line(aes(y = 落後指標), color = "#d95f02", size = 1.5) +
  labs(x = "", y = "", title = "景氣指標", subtitle = "單位：點") + # 兩標、title、subtitle為Y軸
  scale_y_discrete(limits = c(97:103), position = "right") +
  scale_x_date( # breaks
    breaks = seq(as.Date("2019-05-01"), as.Date("2020-03-01"), by = "2 month"),
    labels = c("201905", "07", "09", "11", "202001", "03")
  ) -> line_baseplot

line_baseplot

## ----fig.showtext=TRUE, message=FALSE, warning=FALSE--------------------------
line_baseplot + emajor_themeCol()

## -----------------------------------------------------------------------------
melt(indicator, id.vars = "年月") -> indicator_long # long format
indicator_long %>% 
  ggplot(aes(x = 年月, y = value, color = variable)) +
  geom_hline(yintercept = 100, color = "black", linetype = "longdash", size = 0.3) + # 標準線
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#d95f02")) +
  labs(x = "", y = "", title = "景氣指標", subtitle = "單位：點") + # 兩標、title
  scale_y_discrete(limits = c(97:103), position = "right") +
  scale_x_date( # breaks
    breaks = seq(as.Date("2019-05-01"), as.Date("2020-03-01"), by = "2 month"),
    labels = c("201905", "07", "09", "11", "202001", "03")
  ) -> line_legend_baseplot

line_legend_baseplot

## ----message=FALSE, warning=FALSE---------------------------------------------
# line_legend_baseplot + emajor_themeCol()

## ----message=FALSE, warning=FALSE---------------------------------------------
point_baseplot <- {
  trash %>%
    ggplot(
      aes(x = trash$統計期, y = trash$`平均每人每日一般廢棄物產生量 (公斤)`, color = trash$統計區)
    ) +
    geom_smooth(size = 0.6, alpha = 0.2) +
    geom_point(size = 0.7) +
    scale_y_continuous(position = "right") + # y 軸靠右
    labs(x = "", y = "", title = "平均每人每日一般廢棄物產生量", subtitle = "單位：公斤") +
    scale_x_date( # breaks
      breaks = c(as.Date("2001-01-01"), seq(as.Date("2005-01-01"), as.Date("2020-01-01"), by = "5 year")),
      labels = c("200101", "200501", "201001", "201501", "202001")
    ) # 兩標、title、subtitle為Y軸
}

point_baseplot

## -----------------------------------------------------------------------------
point_baseplot + emajor_themeCol()

