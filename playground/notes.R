# notes
# total working days 2023 Brandenburg from February onwards: 
wd = 252
hollidays = 29
tot_wd = wd-hollidays
ho_wd = tot_wd*0.6
month <-      c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
wd_ho_used <- c(8, 10, 3, 0, 0, 0, 0, 0, 0, 0,  0,  0)
already_used_ho_sum = sum(wd_ho_used)
#ho_planned_04_spain = 5 --> in used for april
#ho_planned_0809_FR_SP = 10 --> in used for july
#ho_planned_12_warm = 10  --> used for december sara & luica
#ho_planned_12_christmas = 5 --> no need because of hollidays
# ho_planned_tot = ho_planned_12_warm + ho_planned_12_christmas
weeks_away = 2+2+1 # weeks taht i am spending all days in homeoffice

remainung_ho_days <- ho_wd - (already_used_ho_sum + weeks_away*4)
current_KW_week <- 13 # 11.12.2023 - 17.12.2023
rem_ho_days_week <- remainung_ho_days/(52 - (current_KW_week))
rem_ho_days_week

sum(wd_ho_used)
