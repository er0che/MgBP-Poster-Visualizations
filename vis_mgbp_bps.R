############## get libs and datasets ############
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
############# import data #############
bl <- read_csv('/studies/Mag/MgBP ABP Data/bl_abp_fixed.csv')
wk6 <- read_csv('/studies/Mag/MgBP ABP Data/wk6_abp_fixed.csv')
wk12 <- read_csv('/studies/Mag/MgBP ABP Data/wk12_abp_fixed.csv')

bl <- rename(bl, 'bl_systolic'=systolic, 'bl_diastolic'=diastolic, 'bl_hr'=hr, 'bl_date'=date, 'bl_time'=time,
             'bl_timesleep'=time_sleep, 'bl_timewake'=time_wake, 'bl_qq_completed'=qq_completed)
wk6 <- rename(wk6, 'wk6_systolic'=systolic, 'wk6_diastolic'=diastolic, 'wk6_hr'=hr, 'wk6_date'=date,
              'wk6_time'=time, 'wk6_timesleep'=time_sleep, 'wk6_timewake'=time_wake, 'wk6_qq_completed'=qq_completed)
wk12 <- rename(wk12, 'wk12_systolic'=systolic, 'wk12_diastolic'=diastolic, 'wk12_hr'=hr, 'wk12_date'=date, 
               'wk12_time'=time, 'wk12_timesleep'=time_sleep, 'wk12_timewake'=time_wake, 'wk12_qq_completed'=qq_completed)

bl_form <- read_csv('/studies/Mag/REDCap_Exports/MgBPClinicForms_DATA_BaselineForm_2019-09-27.csv')
wk6_form <- read_csv('/studies/Mag/REDCap_Exports/MgBPClinicForms_DATA_6wkVISIT_2020-01-02.csv')
wk12_form <- read_csv('/studies/Mag/REDCap_Exports/MgBPClinicForms_DATA_12wkVISIT_2020-01-02.csv')

bl_form <- select(bl_form, record_id, bv_q1b_bp_systolic_1, bv_q1c_bp_systolic_2,
                  bv_q1d_bp_systolic_3, bv_q1b_bp_diastolic_1, bv_q1c_bp_diastolic_2, bv_q1d_diastolic_3)
wk6_form <- select(wk6_form, record_id, fu_q1c_bp_systolic_1, fu_q1d_bp_systolic_2, fu_q1e_bp_systolic_3, 
                   fu_q1c_bp_diastolic_1, fu_q1d_bp_diastolic_2, fu_q1e_bp_diastolic_3)
wk12_form <- select(wk12_form, record_id, fu_q1c_bp_systolic_1, fu_q1d_bp_systolic_2, fu_q1e_bp_systolic_3, 
                    fu_q1c_bp_diastolic_1, fu_q1d_bp_diastolic_2, fu_q1e_bp_diastolic_3)

###################### merge #########

bl_24_avg <- aggregate(bl[, 2], list(bl$ID), mean)
bl_24_avg <- rename(bl_24_avg, ID=Group.1)
bl_24d_avg <- aggregate(bl[, 3], list(bl$ID), mean)
bl_24d_avg <- rename(bl_24d_avg, ID=Group.1)
bl_24_avg <- merge(bl_24_avg, bl_24d_avg, by='ID', all=T)

wk6_24_avg <- aggregate(wk6[, 2], list(wk6$ID), mean)
wk6_24_avg <- rename(wk6_24_avg, ID=Group.1)
wk6_24d_avg <- aggregate(wk6[, 3], list(wk6$ID), mean)
wk6_24d_avg <- rename(wk6_24d_avg, ID=Group.1)
wk6_24_avg <- merge(wk6_24_avg, wk6_24d_avg, by='ID', all=T)

wk12_24_avg <- aggregate(wk12[, 2], list(wk12$ID), mean)
wk12_24_avg <- rename(wk12_24_avg, ID=Group.1)
wk12_24d_avg <- aggregate(wk12[, 3], list(wk12$ID), mean)
wk12_24d_avg <- rename(wk12_24d_avg, ID=Group.1)
wk12_24_avg <- merge(wk12_24_avg, wk12_24d_avg, by='ID', all=T)

avg <- merge(bl_24_avg, wk6_24_avg, by='ID', all=T)
avg <- merge(avg, wk12_24_avg, by='ID', all=T)

avg <- rename(avg, 'bl_24_systolic'=bl_systolic, 'bl_24_diastolic'=bl_diastolic, 'wk6_24_systolic'=wk6_systolic,
              'wk6_24_diastolic'=wk6_diastolic, 'wk12_24_systolic'=wk12_systolic, 'wk12_24_diastolic'=wk12_diastolic)

wk6_form$record_id <- apply(wk6_form[1], MARGIN=2, function(x) {ifelse(x=='10', '1010', ifelse(x=='14', '1014', x))})
wk12_form$record_id <- apply(wk6_form[1], MARGIN=2, function(x) {ifelse(x=='10', '1010', ifelse(x=='14', '1014', x))})

bl_form$bl_s_systolic <- rowMeans(bl_form[2:4], na.rm=TRUE)
bl_form$bl_s_diastolic <- rowMeans(bl_form[5:7], na.rm=TRUE)
wk6_form$wk6_s_systolic <- rowMeans(wk6_form[2:4], na.rm=TRUE)
wk6_form$wk6_s_diastolic <- rowMeans(wk6_form[5:7], na.rm=TRUE)
wk12_form$wk12_s_systolic <- rowMeans(wk12_form[2:4], na.rm=TRUE)
wk12_form$wk12_s_diastolic <- rowMeans(wk12_form[5:7], na.rm=TRUE)

bl_form <- select(bl_form, record_id, bl_s_systolic, bl_s_diastolic)
wk6_form <- select(wk6_form, record_id, wk6_s_systolic, wk6_s_diastolic)
wk12_form <- select(wk12_form, record_id, wk12_s_systolic, wk12_s_diastolic)

avg <- merge(avg, bl_form, by.x='ID', by.y='record_id')
avg <- merge(avg, wk6_form, by.x='ID', by.y='record_id')
avg <- merge(avg, wk12_form, by.x='ID', by.y='record_id')

rand <- read_csv('mag_rand.csv')
rand <- select(rand, record_id, Treatment)
avg <- merge(avg, rand, by.x='ID', by.y='record_id')

avg <- as.data.frame(apply(avg, MARGIN=2, function(x) {ifelse(is.nan(x), NA, x)}))

############### differences #########
avg[2:13] <- apply(avg[2:13], MARGIN=2, function(x) as.numeric(x))

avg$blto6_24_s_diff <- avg$bl_24_systolic - avg$wk6_24_systolic
avg$blto6_24_d_diff <- avg$bl_24_diastolic - avg$wk6_24_diastolic
avg$blto12_24_s_diff <- avg$bl_24_systolic - avg$wk12_24_systolic
avg$blto12_24_d_diff <- avg$bl_24_diastolic - avg$wk12_24_diastolic

avg$blto6_s_s_diff <- avg$bl_s_systolic - avg$wk6_s_systolic
avg$blto6_s_d_diff <- avg$bl_s_diastolic - avg$wk6_s_diastolic
avg$blto12_s_s_diff <- avg$bl_s_systolic - avg$wk12_s_systolic
avg$blto12_s_d_diff <- avg$bl_s_diastolic - avg$wk12_s_diastolic

avg_a <- filter(avg, Treatment=='A')
avg_b <- filter(avg, Treatment=='B')

tps <- c(0,6,12)
A <- data.frame(group = "A", avgs=c(mean(avg_a$bl_24_systolic, na.rm=TRUE), mean(avg_a$wk6_24_systolic, na.rm=TRUE), 
                                    mean(avg_a$wk12_24_systolic, na.rm=TRUE)), tp=tps,
                avgs2=c(mean(avg_a$bl_s_systolic, na.rm=TRUE), mean(avg_a$wk6_s_systolic, na.rm=TRUE),
                        mean(avg_a$wk12_s_systolic, na.rm=TRUE)))
B <- data.frame(group='B', avgs=c(mean(avg_b$bl_24_systolic, na.rm=TRUE), mean(avg_b$wk6_24_systolic, na.rm=TRUE), 
                mean(avg_b$wk12_24_systolic, na.rm=TRUE)), tp=tps,
                avgs2=c(mean(avg_b$bl_s_systolic, na.rm=TRUE), mean(avg_b$wk6_s_systolic, na.rm=TRUE),
                        mean(avg_b$wk12_s_systolic, na.rm=TRUE)))

both <- rbind(A, B)

ggplot(both, aes(tp, col = group)) +  geom_line(aes(y = avgs)) + geom_line(aes(y=avgs2), linetype='dashed')

tps <- c(6,12)
A <- data.frame(group = "A", avgs=c(-mean(avg_a$blto6_24_s_diff, na.rm=TRUE), -mean(avg_a$blto12_24_s_diff, na.rm=TRUE)), 
                                    tp=tps, avgs2=c(-mean(avg_a$blto6_s_s_diff, na.rm=TRUE), 
                                                   -mean(avg_a$blto12_s_s_diff,na.rm=TRUE)))
B <- data.frame(group='B', avgs=c(-mean(avg_b$blto6_24_s_diff, na.rm=TRUE), -mean(avg_b$blto12_24_s_diff, na.rm=TRUE)), 
                                  tp=tps, avgs2=c(-mean(avg_b$blto6_s_s_diff, na.rm=TRUE), 
                                                 -mean(avg_b$blto12_s_s_diff,na.rm=TRUE)))

both <- rbind(A, B)

ggplot(both, aes(tp, col = group)) +  geom_line(aes(y = avgs)) + geom_line(aes(y=avgs2), linetype='dashed')

