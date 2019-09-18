library(tidyverse)
library(lubridate)

df_districts <- read_csv(here::here("2019HKsuicide", "Hong_Kong_18_Districts.csv"))

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
                theme_minimal() + 
                labs(x = "", y = "", 
                     title = "香港2019自殺個案情況", 
                     subtitle = subtitle, 
                     caption = caption) + 
                theme(plot.margin = margin(20, 10, 20, 10), 
                      plot.title = element_text(face = "bold", size = 24), 
                      plot.subtitle = element_text(size = 16), 
                      plot.caption = ggtext::element_markdown(size = 12),
                      text = element_text(family = "STXihei"), 
                      panel.grid = element_blank(), 
                      strip.text = element_text(size = 15)) -> alluvial
)
if(!file.exists(here::here("2019HKsuicide", "alluvial.png")))
        ggsave(here::here("2019HKsuicide", "alluvial.png"), alluvial, 
               width = 36.9, height = 19.9, units = "cm")


# ----2----
# |============================================|
# |                     time                   |
# |============================================|
library(ggridges)
df_time <- df_record %>% 
        filter(deceased == "Yes") %>% 
        select(date, week, day, age, hour)

ridge <- df_time %>% 
        group_by(month = month(date)) %>% 
        count(date) %>% 
        ungroup() %>% 
        ggplot(aes(x = n, y = factor(month))) + 
        geom_density_ridges(aes(fill = factor(month)), alpha = .6, show.legend = FALSE) 


ridge_density_lines <- 
        ggplot_build(ridge) %>% 
        pluck("data", 1) %>% 
        group_by(group) %>% 
        filter(density == max(density)) %>% 
        ungroup()

ridge + geom_segment(data = ridge_density_lines, linetype = 2,
                     aes(x = x, xend = x, y = ymin, yend = ymin + density * scale * iscale)) + 
        scale_y_discrete(labels = month.abb[1:9]) + 
        coord_flip() +
        labs(x = "", y = "") +
        theme_minimal() + 
        theme(plot.margin = margin(t = 20), 
              panel.grid = element_blank(), 
              panel.grid.major.y = element_line(color = "grey 80", size = .18))

df_month <- 
        df_time %>% 
        group_by(month = month(date, label = TRUE, abbr = TRUE)) %>% 
        count(date, name = "count") %>% 
        ungroup() %>% 
        mutate(quarter = as_factor(case_when(month %in% month.abb[1:3] ~ "1st quarter",
                                             month %in% month.abb[4:6] ~ "2nd quarter",
                                             month %in% month.abb[7:9] ~ "3rd quarter")))
df_month$month <- droplevels(df_month$month)

gplots::plotmeans(count ~ quarter, data = df_month, frame = FALSE, use.t = TRUE, p = .9)
df_month_aov <- aov(count ~ quarter, data = df_month)
summary(df_month_aov)
df_month_aov %>% 
        TukeyHSD() %>% 
        pluck("quarter") %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "pair")

