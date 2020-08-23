# plan_emajor------------
plan_emajor=drake::drake_plan(
# > plan begins -----------
# >> setup--------------
knitr::opts_chunk$set(echo = TRUE, message=F, warning=F)
library(drake)
purl_drakePlan(
  filename=
    file.path(
      getwd(),
      "emajor.Rmd"),
  plan_name="plan_emajor"
),

# >> Report--------------
Report=read_excel(file_in("/Users/martin/Github/emajor/Report.xls")),

# >> deposit--------------
deposit=
{
  Report%>%
    pivot_longer(
      cols=`中華民國`:`德　國`,
      names_to="國別",values_to="外匯存底"
    )%>%
    filter(時間=="109年5月")->deposit
  
  deposit[order(-deposit$外匯存底),]->deposit
  
  deposit%>%
    mutate(order=(as.character(1:7)))->deposit
},

# >> trash--------------
trash = {
  trash <- read_excel(file_in("/Users/martin/Github/emajor/trash.xlsx"))
  trash$統計期 %>% ymd() -> trash$統計期
  trash
},

# >> indicator1--------------
indicator1 = {
  read_excel(file_in("/Users/martin/Github/emajor/indicator.xlsx")) -> indicator
  indicator$年月 %>% ymd() ->indicator$年月
  melt(indicator, id.vars = "年月")->indicator1 #long format
  indicator1
},

# >> line_baseplot--------------
line_baseplot = {
indicator%>%ggplot(aes(x=年月))+
  geom_hline(yintercept = 100, color="black", linetype="longdash" , size =0.3)+ #標準線
  geom_line(aes(y=領先指標),color="#1f78b4",size=1.5)+ #geom
  geom_line(aes(y=同時指標),color="#33a02c",size=1.5)+
  geom_line(aes(y=落後指標),color="#d95f02",size=1.5)+
  labs(x="",y="",title = "景氣指標", subtitle = "單位：點")+ #兩標、title、subtitle為Y軸
  scale_y_discrete(limits = c(97:103), position = "right")+
  scale_x_date( #breaks
      breaks = seq(as.Date("2019-05-01"), as.Date("2020-03-01"), by="2 month"),
      labels = c("201905","07","09","11","202001","03")
      )
},

# >> line_legend_baseplot--------------
line_legend_baseplot = {
  indicator1%>%ggplot(aes(x=indicator1$年月,y=indicator1$value,color=indicator1$variable))+
  geom_hline(yintercept = 100, color="black", linetype="longdash" , size =0.3)+ #標準線
  geom_line(size=1.5)+
  scale_color_manual(values = c("#1f78b4","#33a02c","#d95f02"))+
  labs(x="",y="",title = "景氣指標", subtitle = "單位：點")+ #兩標、title
  scale_y_discrete(limits = c(97:103), position = "right")+
  scale_x_date( #breaks
      breaks = seq(as.Date("2019-05-01"), as.Date("2020-03-01"), by="2 month"),
      labels = c("201905","07","09","11","202001","03")
      )
},

# >> col_baseplot--------------
col_baseplot = {
  deposit%>%
  ggplot()+
  geom_col(
    aes(x=order,y=外匯存底),
    width = 0.5,fill="#1f78b4"
  )+
  labs(x="",y="",title = "主要國家於2020年5月的外匯存底", subtitle = "單位：億美元")+
  scale_y_continuous(position = "right")+
  scale_x_discrete(labels=deposit$國別)

},

# >> point_baseplot--------------
point_baseplot = {
  trash %>% ggplot(
  aes(x=trash$統計期, y= trash$`平均每人每日一般廢棄物產生量 (公斤)`, color=trash$統計區)
  )+
  geom_smooth(size = 0.6, alpha = 0.2)+
  geom_point(size=0.7)+
  scale_y_continuous(position = "right")+ #y 軸靠右
  labs(x="",y="",title = "平均每人每日一般廢棄物產生量", subtitle = "單位：公斤")+
  scale_x_date( #breaks
      breaks = c(as.Date("2001-01-01") ,seq(as.Date("2005-01-01"), as.Date("2020-01-01"), by="5 year")),
      labels = c("200101","200501","201001","201501","202001")
      )  #兩標、title、subtitle為Y軸
},

# >> line_baseplot_m--------------
line_baseplot_m ={
  emajor_theme(line_baseplot,0)
}

# > plan ends ------------
)

# make plan -----------------
mk_plan_emajor = function(){
# no params in the frontmatter

library(dplyr); library(stringr); library(ggplot2); library(plotly); library(lubridate); library(readr); library(tidyr); library(showtext);library(grid);library(readxl);library(gridExtra);
library(reshape2); library(emajor)

font_add_google("Noto Serif TC", "NSerifC") #google字體
showtext_auto(enable=TRUE)#啟用字體

  drake::make(plan_emajor)
}

