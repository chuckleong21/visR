library(tidyverse)
library(lubridate)

df_districts <- readr::read_csv(here::here("2019HKsuicide", "Hong_Kong_18_Districts.csv"))

df_hk <- fortify(rgdal::readOGR(here::here("2019HKsuicide", "Hong_Kong_18_Districts")))
df_hk %<>% 
        transform(id = as.numeric(id) + 1) %>% 
        left_join(df_districts, by = c("id" = "OBJECTID")) %>% 
        rename(district = "TCNAME") %>% 
        select(1, 2, 5, 7, 8)

df_record <- jsonlite::fromJSON("https://api.sheety.co/0877de29-6a38-44bd-a7c4-dfade476adef?fbclid=IwAR2R-qhvzumnUrle_g6z1rJiKR3pqlSGAR-dUw9MMLkRFqyuby6N_N9EPHI")
df_record %<>% 
        separate(`個案發現日期`, c("date", "day"), " ") %>% 
        rename(time = "時間", district = "十八區", area = "地區", street = `街道/屋邨/建築`,
               site = `門牌/樓宇/地點`, lat.loc = "緯度", long.loc = "經度", housetype = "房屋", 
               sex = "性別", age = "年齡", reason_1 = "原因1", reason_2 = "原因2", 
               reason_3 = "原因3", deceased = "死亡", cause = "類別", source = "報導來源") %>% 
        pivot_longer(starts_with("reason_"), names_to = "reason_n", names_prefix = "reason_", 
                     values_to = "reason") %>% 
        mutate(age = as.numeric(str_remove(age, "歲")), 
               age = ifelse(is.na(age), -1, age),
               deceased = factor(ifelse(deceased == "是", "Yes", "No")), 
               date = ymd(date), 
               day = wday(date, label = TRUE, abbr = TRUE, week_start = 1), 
               week = week(date), 
               day_interval = substr(time, 1, 2), 
               hour = as.numeric(str_extract(time, "\\d+")), 
               hour = ifelse(day_interval %in% c("早上", "中午", "清晨", "上午", "凌晨"),
                             hour, hour + 12),
               hour = ifelse(is.na(hour) | hour > 24, 24, hour), 
               antielab = factor(ifelse(date >= as.Date("2019-06-01") & date <= max(date), 
                                 "After", "Before"))) %>% 
        filter(!is.na(reason)) %>% 
        select(-source, -time, -reason_n, -day_interval)

# ----1----
# |=================================================================|
# |                     demographic alluvial                        |
# |=================================================================|
library(ggalluvial)
library(ggtext)
df_demographic <- 
        select(df_record, date, sex, age, reason) %>% 
        filter(!reason == "不詳", age >= 18) %>% 
        mutate(age = case_when(between(age, 18, 30) ~ "18-30歲", 
                               between(age, 31, 50) ~ "31-50歲",
                               between(age, 51, 70) ~ "51-70歲", 
                               age > 70 ~ "70歲以上")) %>% 
        group_by(month = month(date, label = TRUE, abbr = TRUE), sex, age, reason) %>%
        add_count(name = "freq") %>% 
        ungroup()

subtitle <- "香港2019年度自殺個案自3月份有明顯上升，6、7月份期間更高達300宗，過後有回到3月份水平。
自殺個案中自殺者為男士居多，且主因大多與健康有關，而自殺者的年齡集中在30歲以上的人士。
除最主要的健康問題外，其中年輕人多為感情問題而有自殺念頭，中年人則為財政生活問題所困。"
caption <- "**數據來源：2019香港自殺資料統計** | 設計：@chucc900<br><span style='color:#F57D7D'> 如需幫助請致電香港撒瑪利亞防止自殺會：2389 - 2222</span>"

(
        ggplot(df_demographic, aes(axis1 = sex, axis2 = age, y = freq)) + 
                scale_x_discrete(limits = c("", ""), expand = c(-.3, 0.45)) +
                geom_flow(aes(fill = reason)) + 
                geom_stratum() + 
                geom_text(stat = "stratum", label.strata = TRUE, family = "STXihei") + 
                scale_fill_brewer(name = "主因", type = "qual", palette = "Set3") +
                facet_wrap(~month, scales = "free_y", nrow = 3) + 
                theme_minimal(base_family = "STXihei") + 
                labs(x = "", y = "", 
                     title = "香港2019自殺個案情況", 
                     subtitle = subtitle, 
                     caption = caption) + 
                theme(plot.margin = margin(20, 10, 20, 10), 
                      plot.title = element_text(face = "bold", size = 24), 
                      plot.subtitle = element_text(size = 16), 
                      plot.caption = element_markdown(size = 12),
                      text = element_text(family = "STXihei"), 
                      panel.grid = element_blank(), 
                      strip.text = element_text(size = 15)) -> alluvial
)
if(!file.exists(here::here("2019HKsuicide", "alluvial.png"))) {
        ggsave(here::here("2019HKsuicide", "alluvial.png"), alluvial, 
               width = 36.9, height = 19.9, units = "cm")
}


# ----2----
# |============================================|
# |                     time                   |
# |============================================|
library(ggridges)

df_time <- df_record %>% 
        filter(deceased == "Yes") %>% 
        select(date, week, day, age, hour)


(
        ridge <- 
                df_time %>% 
                group_by(month = month(date)) %>% 
                count(date) %>% 
                ungroup() %>% 
                ggplot(aes(x = n, y = factor(month))) + 
                geom_density_ridges(aes(fill = factor(month)), alpha = .6, show.legend = FALSE)
)


ridge_density_lines <- 
        ggplot_build(ridge) %>% 
        pluck("data", 1) %>% 
        group_by(group) %>% 
        filter(density == max(density)) %>% 
        ungroup()

df_month <- 
        df_time %>% 
        group_by(month = month(date, label = TRUE, abbr = TRUE)) %>% 
        count(date, name = "count") %>% 
        ungroup() %>% 
        mutate(quarter = as_factor(case_when(month %in% month.abb[1:3] ~ "第一季度",
                                             month %in% month.abb[4:6] ~ "第二季度",
                                             month %in% month.abb[7:9] ~ "第三季度")))
df_month$month <- droplevels(df_month$month)

#                       oneway ANOVA test
df_quarter_aov <- aov(count ~ quarter, data = df_month) 
car::leveneTest(count ~ quarter, df_month) # cannot reject heteroskedasticity
df_quarter_aov <- oneway.test(count ~ quarter, df_month) # use Welch t.test
multcomp::glht(df_quarter_aov, linfct = multcomp::mcp(quarter = "Tukey"))

df_aov <- data.frame(x = c(rep(7.45, 3), rep(7.65, 3), rep(9, 3), 
                           8.3, 8.95, 8.95, 8.3, 
                           9, 10.15, 10.15, 9.5),
                     y = c(1:9 + .5, 
                           2.5, 2.5, 5.5, 5.5, 
                           5.8, 5.8, 8.5, 8.5), 
                     group = c(rep(1:3, each = 3), rep(4, 4), rep(5, 4)))

#       draw a summary table
# tt <- ttheme_default(base_family = "STXihei")
# df_mean_summary <- df_month %>%
#         group_by(`月份` = month) %>%
#         summarize(`月均自殺身亡個案` = round(mean(count), 2)) %>%
#         tableGrob(rows = NULL, theme = tt)
# grid.newpage()
# grid.draw(df_mean_summary) 

subtitle <- "下圖系單日自殺身亡案宗數的頻率分佈的峰巒圖。<br>圖形顯示頻率均值出現**右移**，
自6月份起分佈出現了更多的**厚尾**——說明自殺身亡在同日發生的情況比以往更頻繁。
<br>季度與季度相比，在95%的置信區間下, 季度均值有顯著性的不同。而進一步檢驗則發現，
<br>**第二季度的均值都比第一季度和第三季度高**，第一季度和第三季度均值則無顯著差異。"

ridge + 
        geom_segment(data = ridge_density_lines, linetype = 2,
                     aes(x = x, xend = x, 
                         y = ymin, yend = ymin + density * scale * iscale)) +  
        geom_path(data = df_aov, aes(x, y, group = group)) +
        annotate("text", x = c(10, 10.8), y = c(4, 7.3), label = c("**", "*"), size = 8) + 
        annotate("text", x = c(10, 10.8), y = c(3, 8.5), 
                 label = c("mu[Q1]<=mu[Q2]", "mu[Q2]>=mu[Q3]"), 
                 parse = TRUE, hjust = c(0, 1), size = 6) + 
        # annotation_custom(df_mean_summary, xmin = -9, xmax = -4, ymin = 9, ymax = 11) + 
        scale_x_reverse(breaks = seq(0, 10, 2), expand = c(0.1, 0)) + 
        scale_y_discrete(labels = month.abb[1:nlevels(df_month$month)]) +
        theme_minimal(base_family = "STXihei") + 
        theme(plot.margin = margin(20, 10, 20, 10), 
              plot.title = element_text(size = 24, face = "bold"),
              plot.subtitle = element_markdown(size = 16),
              plot.caption = element_markdown(size = 12),
              panel.grid = element_blank(), 
              panel.grid.major.y = element_line(color = "grey80", size = .18), 
              axis.text = element_text(size = 10), 
              axis.title.y = element_text(size = 12)) + 
        coord_flip() + 
        labs(x = "單日自殺身亡案宗數", y = "", 
             title = "香港2019年自殺情況",
             subtitle = subtitle, caption = caption)

if(!file.exists(here::here("2019HKsuicide", "ridge.png"))) {
        ggsave(here::here("2019HKsuicide", "ridge.png"), 
               width = 36.9, height = 19.9, units = "cm")
}
