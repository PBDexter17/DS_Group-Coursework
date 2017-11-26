# **************************************
# load data
# **************************************
df_train = read_csv("/Users/sujie/Desktop/KaggleGroupWork/kaggle-airbnb-recruiting-new-user-bookings-master/data/train_users_2.csv",
                    col_types = cols(
                      timestamp_first_active = col_character()))
df_test = read_csv("/Users/sujie/Desktop/KaggleGroupWork/kaggle-airbnb-recruiting-new-user-bookings-master/data/test_users.csv",
                   col_types = cols(
                     timestamp_first_active = col_character()))
#get labels
labels = df_train[, c('id', 'country_destination')]
#create a column in test table 
df_test$country_destination = NA
age_gender_bkts <- fread("/Users/sujie/Desktop/KaggleGroupWork/kaggle-airbnb-recruiting-new-user-bookings-master/data/age_gender_bkts.csv", data.table=F)
countries <- fread("/Users/sujie/Desktop/KaggleGroupWork/kaggle-airbnb-recruiting-new-user-bookings-master/data/countries.csv", data.table=F)
sample_submission_NDF <- fread("/Users/sujie/Desktop/KaggleGroupWork/kaggle-airbnb-recruiting-new-user-bookings-master/data/sample_submission_NDF.csv", data.table=F)
sessions <- fread("/Users/sujie/Desktop/KaggleGroupWork/kaggle-airbnb-recruiting-new-user-bookings-master/data/sessions.csv", data.table=F)

# combine train and test data
df_train$dataset <- "train"
df_test$dataset <- "test"

#transform the type of test date account created column from(YYYY-MM-DD) into (YYYY/MM/DD)
#as.Date: A character string. If not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" on the first non-NA element, and give an error if neither works.
df_test$date_account_created <- format(as.Date(df_test$date_account_created), "%Y/%m/%d")
# combine train and test data

df_all = rbind(df_train, df_test)

# **************************************
# clean age
# **************************************
#%>% meaning: For example  iris %>% head() %>% summary() is equals  to summary(head(iris))
df_all <- df_all %>%
  dplyr::mutate(
    #clean bad age data which is define in year such as 2014 or ...
    #using this data is collect in 2015 so we use 2015 as a point to calculate the age who
    #use birth year as their date. so the age_cln feature means real age
    age_cln = ifelse(age >= 1920, 2015 - age, age),
    #create a feature wihch define the childrens and people whose age is greater than 100 as -1
    #others keep real age
    age_cln2 = ifelse(age_cln < 14 | age_cln > 100, -1, age_cln),
    #cut the age feature into several parts but using min value of feature age_cln and max value of it.
    #age_bucket will be a table which store the data like {age cloumn:"10" age_bucket="(4,9]"}
    age_bucket = cut(age, breaks = c(Min(age_cln), 4, 9, 14, 19, 24,
                                     29, 34, 39, 44, 49, 54,
                                     59, 64, 69, 74, 79, 84,
                                     89, 94, 99, Max(age_cln)
    )),
    #change the type of age_bucket map it from "(1,4]" into "0-4" after that the age_bucket shows like that
    #{age cloumn:"10" age_bucket column":"4-9"}
    #mapv form pylr package: Item in x that match items from will be replaced by items in to, matched by position
    age_bucket = mapvalues(age_bucket,
                           from=c("(1,4]", "(4,9]", "(9,14]", "(14,19]",
                                  "(19,24]", "(24,29]", "(29,34]", "(34,39]",
                                  "(39,44]", "(44,49]", "(49,54]", "(54,59]",
                                  "(59,64]", "(64,69]", "(69,74]", "(74,79]",
                                  "(79,84]", "(84,89]", "(89,94]", "(94,99]", "(99,150]"),
                           to=c("0-4", "5-9", "10-14", "15-19",
                                "20-24", "25-29", "30-34", "35-39",
                                "40-44", "45-49", "50-54", "55-59",
                                "60-64", "65-69", "70-74", "75-79",
                                "80-84", "85-89", "90-94", "95-99", "100+"))
  )

# **************************************
# feature using date_first_booking
# **************************************
#%>% means separate(df_train,.....) and dplyr::mutate(separate(df_train,.....),...)
df_all <- df_all %>% 
  #seprate the date_account_created into 3 part and the seprate symbol is "/"
  #seprate function is from "tidyr" package and the function of it is to 
  #Given either regular expression or a vector of character positions, separate() turns a single character column into multiple columns
  separate(date_account_created, c("dac_year", "dac_month", "dac_day"), sep = "/", remove=FALSE) %>% 
  mutate(
    #paste0 : Concatenate vectors after converting to character
    #e.g. dac_year:" 1995" dac_month:"06" then the dac_yearmonth :"199506"
    dac_yearmonth = paste0(dac_year, dac_month),
    #change the dac_yearmonthday into numeric type from "19950602" into 19950602
    dac_yearmonthday = as.numeric(paste0(dac_year, dac_month, dac_day)),
    #as.Date(...,"%U") which can get the date and calculate the week index of the year 
    #one year have 51 weeks and start at the first sunday
    dac_week = as.numeric(format(as.Date(date_account_created)+3, "%U")),
    #formatC(..,..,..) flag = "0" pads leading zeros("1"->"01") width: the total field width
    #create the feature : "dac_yearmonthweek"
    dac_yearmonthweek = as.numeric(paste0(dac_year, dac_month, formatC(dac_week, width=2, flag="0"))),
    #str_sub: get the subString of the timestamp_first_active get 1-4 string charcter of "199501000000"->"1995"
    tfa_year = str_sub(timestamp_first_active, 1, 4),
    #str_sub: get the subString of the timestamp_first_active get 5-6 string charcter of "199501000000"->"01"
    tfa_month = str_sub(timestamp_first_active, 5, 6),
    #str_sub: get the subString of the timestamp_first_active get 7-8 string charcter of "199501000000"->"00"
    tfa_day = str_sub(timestamp_first_active, 7, 8),
    #add 1 to the day because in the next step we need to change it into Date format
    tfa_day = as.character(as.numeric(tfa_day)+1),
    #same as above
    tfa_yearmonth = str_sub(timestamp_first_active, 1, 6),
    tfa_yearmonthday = as.numeric(str_sub(timestamp_first_active, 1, 8)),
    #create the Date type data from "1995","01""01" -> "1995-01-01"
    tfa_date = as.Date(paste(tfa_year, tfa_month,tfa_day, sep="-")),
    #get week
    tfa_week = as.numeric(format(as.Date(tfa_date)+3, "%U")),
    #same as above
    tfa_yearmonthweek = as.numeric(paste0(tfa_year, tfa_month, formatC(tfa_week, width=2, flag="0"))),
    #create the feature as the period of account create and first active
    dac_lag = as.numeric(as.Date(date_account_created) - tfa_date),
    #ceate the feature as the period of  first booking and data account created
    dfb_dac_lag = as.numeric(as.Date(date_first_booking) - as.Date(date_account_created)),
    #cut2 is from Hmisc package 
    #Function like cut but left endpoints are inclusive and labels are of the form [lower, upper), except that last interval is [lower,upper]. 
    #and it will be cut into [lower,0) 0 [1,365] Na
    dfb_dac_lag_cut = as.character(cut2(dfb_dac_lag, c(0, 1))),
    #Levels is define like taht :    0 [1, 365] [-349,0)  NA
    #                      level:    1    2        3      4
    #and change it into level_after: 0    1        2      3
    dfb_dac_lag_flg = as.numeric(as.factor(ifelse(is.na(dfb_dac_lag_cut)==T, "NA", dfb_dac_lag_cut))) - 1,
    dfb_tfa_lag = as.numeric(as.Date(date_first_booking) - as.Date(tfa_date)),
    dfb_tfa_lag_cut = as.character(cut2(dfb_tfa_lag, c(0, 1))),
    #level is = 0 [1,1385] NA [1,2,3] -> [0,1,2]
    #same as above
    dfb_tfa_lag_flg = as.numeric(as.factor(ifelse(is.na(dfb_tfa_lag_cut)==T, "NA", dfb_tfa_lag_cut))) - 1
    )
# **************************************
# join countries
# **************************************
#add one column into the countries: language which is the first two character of the destination_language
countries <- dplyr::mutate(countries,
                           language = str_sub(destination_language, 1, 2))
df_all <- df_all %>%
  #can not understand the 85 line
  #no need
  #using the countries date and map them into the df_all date by language and country_destination
  #function describe : left_join(x,y,by = null)
  #x and y should usually be from the same data source, but if copy is TRUE, y will automatically be copied to the same source as x.
  dplyr::mutate(country_destination = country_destination) %>%
  dplyr::left_join(., countries[c("language", "country_destination", "distance_km", "destination_km2", "language_levenshtein_distance")],
                   by = c("language", "country_destination"))
#delet the language column in countries dataset
countries$language <- NULL


# **************************************
# set validation
# **************************************
#create the train set which dataset type equals 'train'
df_train <- subset(df_all, dataset == "train")
#seperate the trainset into training set and validation set
df_train <- df_train %>%
  #if dac_yearmonth is not in ("201404", "201405", "201406") then set it to train otherwise valid
  dplyr::mutate(dataset = ifelse(dac_yearmonth %nin% c("201404", "201405", "201406"), "train", "valid"))
#create test set
df_test <- subset(df_all, dataset == "test")
#combine it into de_all, now df_all have three set
df_all = rbind(df_train, df_test)


# **************************************
# stack numeric feature
# **************************************
num_feats <- c(
  "age_cln",
  "age_cln2",
  "dac_year",
  "dac_month",
  "dac_yearmonth",
  "dac_yearmonthday",
  "dac_yearmonthweek",
  "dac_day",
  "dac_week",
  "tfa_year",
  "tfa_month",
  "tfa_yearmonth",
  "tfa_yearmonthday",
  "tfa_yearmonthweek",
  "tfa_day",
  "tfa_week"#,
  # "dac_lag",
  # "dfb_dac_lag",
  # "dfb_tfa_lag"
)
#define a  list
df_all_num_feats <- list()
i <- 1
#traverse the num_feature
for(feat in num_feats){
  #e.g. select the id and age_cln from df_all and create a new table
  df_all_num_feats_ <- df_all[c("id", feat)]
  #create new column and now the table is like {id:"xx" age_cln:"35" feature:"age_cln"}
  df_all_num_feats_$feature <- feat
  #add new column {id:"xx" age_cln:"35" feature:"age_cln" value: 35}
  df_all_num_feats_$value <- as.numeric(df_all_num_feats_[[feat]])
  #select three column and renew the table : {id:"xx" feature:"age_cln" value: 35}
  df_all_num_feats_ <- df_all_num_feats_[c("id", "feature", "value")]
  #add it into list
  df_all_num_feats[[i]] <- df_all_num_feats_
  i <- i + 1
}
#combine all list
df_all_num_feats <- bind_rows(df_all_num_feats)
print("numeric feature")
print(n_distinct(df_all_num_feats$feature))


# **************************************
# stack categorical feature
# **************************************
#similar as above
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
#define list
df_all_ohe_feats <- list()
i <- 1
n_feats <- 0
#traverse the categorical feature
for(feat in ohe_feats){
  df_all_ohe_feats_ <- df_all[c("id", feat)]
  #create feature column as {id:"xx",gender:"male",feature:"gender_male"}
  df_all_ohe_feats_$feature <- paste(feat, df_all_ohe_feats_[[feat]], sep="_")
  #n_feats_ = "gender_male"
  n_feats_ <- n_distinct(df_all_ohe_feats_$feature)
  #set value to 1 and the table update as {id:"xx",gender:"male",feature:"gender_male",value:1}
  df_all_ohe_feats_$value <- 1
  #select three column and renew table {id:"xx",feature:"gender_male",value:1}
  df_all_ohe_feats_ <- df_all_ohe_feats_[c("id", "feature", "value")]
  #add to list
  df_all_ohe_feats[[i]] <- df_all_ohe_feats_
  i <- i + 1
  n_feats <- n_feats + n_feats_
}
#combine them
df_all_ohe_feats <- bind_rows(df_all_ohe_feats)
print("categorical feature")
print(n_feats)


# **************************************
# sessions features
# **************************************
sessions$flg <- 1 #add 1 to all rows
sessions <- data.table(sessions) #subset rows, select and compute on columns and perform aggregations by group
sessions[, seq := sequence(.N), by = c("user_id")] #add feature "seq", the value is decided by the count of the same "user_id".
sessions[, seq_rev := rev(sequence(.N)), by = c("user_id")] #add feature "seq_rev", the value is the reverse of the count.
sessions[, action2 := paste(action, action_type, action_detail, device_type, sep="_"),] #add feature "action2" which is joined by all other features except ""secs_elapsed,eg:"index_view_view_search_results_Windows Desktop"

first_execution <- 1

if(first_execution == 1){
  #new feature "sessions_action_se_sum": sum up all time spent by one user on one action.
  sessions_action_se_sum <- sessions[,list(secs_elapsed_sum = sum(secs_elapsed, na.rm=T)),
                                     by=list(user_id, action)]
  #na.rm = T, logical. Should missing values (including NaN) be removed
  
  #add one feature "variable" and set to null ????no need
  # sessions_action_se_sum <- melt.data.table(sessions_action_se_sum)
  # sessions_action_se_sum$variable <- NULL
  #switch from data.table to data.frame
  sessions_action_se_sum <- data.frame(sessions_action_se_sum)
  #names(): set the names of an object //c(),combines arguments
  names(sessions_action_se_sum) <- c("id", "feature", "value")
  #paste "action_se_sum" and add to feature,join with "_" eg: "lookup" ---> "action_se_sum_lookup"
  sessions_action_se_sum$feature <- paste("action_se_sum", sessions_action_se_sum$feature, sep="_")
  
  install.packages("dplyr")
  library(dplyr)
  #n_distinct(), This is a faster and more concise equivalent of length(unique(x))
  n_distinct(sessions_action_se_sum$feature) #360
  #save cache
  saveRDS(sessions_action_se_sum, "cache/sessions_action_se_sum.RData")
  
  #new feature "sessions_action_type_se_sum", sum over secs_elapsed, by use_id and action_type
  sessions_action_type_se_sum <- sessions[,list(secs_elapsed_sum = sum(secs_elapsed, na.rm=T)),
                                          by=list(user_id, action_type)]
  ####not necessary stupid!!!
  #sessions_action_type_se_sum <- melt.data.table(sessions_action_type_se_sum)
  #sessions_action_type_se_sum$variable <- NULL
  ####not necessary stupid!!!
  sessions_action_type_se_sum <- data.frame(sessions_action_type_se_sum)
  names(sessions_action_type_se_sum) <- c("id", "feature", "value")
  sessions_action_type_se_sum$feature <- paste("action_type_se_sum", sessions_action_type_se_sum$feature, sep="_")
  n_distinct(sessions_action_type_se_sum$feature)
  saveRDS(sessions_action_type_se_sum, "cache/sessions_action_type_se_sum.RData")
  
  #new feature "sessions_action_detail_se_sum": sum over secs_elapsed, by user_id and action_detail..
  sessions_action_detail_se_sum <- sessions[,list(secs_elapsed_sum = sum(secs_elapsed, na.rm=T)),
                                            by=list(user_id, action_detail)]
  sessions_action_detail_se_sum <- data.frame(sessions_action_detail_se_sum)
  names(sessions_action_detail_se_sum) <- c("id", "feature", "value")
  sessions_action_detail_se_sum$feature <- paste("action_detail_se_sum", sessions_action_detail_se_sum$feature, sep="_")
  n_distinct(sessions_action_detail_se_sum$feature) #156
  saveRDS(sessions_action_detail_se_sum, "cache/sessions_action_detail_se_sum.RData")
  
  #new feature "sessions_device_type_se_sum": sum over secs_elapsed ,by user_id and device_type
  sessions_device_type_se_sum <- sessions[,list(secs_elapsed_sum = sum(secs_elapsed, na.rm=T)),
                                          by=list(user_id, device_type)]
  sessions_device_type_se_sum <- data.frame(sessions_device_type_se_sum)
  names(sessions_device_type_se_sum) <- c("id", "feature", "value")
  sessions_device_type_se_sum$feature <- paste("device_type_se_sum", sessions_device_type_se_sum$feature, sep="_")
  n_distinct(sessions_device_type_se_sum$feature) #14
  saveRDS(sessions_device_type_se_sum, "cache/sessions_device_type_se_sum.RData")
  
  #new feature "sessions_device_type_se_sum": sum over times, by user_id and action
  sessions_action_flg_sum <- sessions[,list(flg_sum = sum(flg, na.rm=T)),
                                      by=list(user_id, action)]
  sessions_action_flg_sum <- data.frame(sessions_action_flg_sum)
  names(sessions_action_flg_sum) <- c("id", "feature", "value")
  sessions_action_flg_sum$feature <- paste("action_flg_sum", sessions_action_flg_sum$feature, sep="_")
  n_distinct(sessions_action_flg_sum$feature) #360
  saveRDS(sessions_action_flg_sum, "cache/sessions_action_flg_sum.RData")
  
  #new feature "sessions_action_type_flg_sum": sum over times, by user_id and action_type
  sessions_action_type_flg_sum <- sessions[,list(flg_sum = sum(flg, na.rm=T)),
                                           by=list(user_id, action_type)]
  sessions_action_type_flg_sum <- data.frame(sessions_action_type_flg_sum)
  names(sessions_action_type_flg_sum) <- c("id", "feature", "value")
  sessions_action_type_flg_sum$feature <- paste("action_type_flg_sum", sessions_action_type_flg_sum$feature, sep="_")
  n_distinct(sessions_action_type_flg_sum$feature)
  saveRDS(sessions_action_type_flg_sum, "cache/sessions_action_type_flg_sum.RData")
  
  #new feature "sessions_action_detail_flg_sum": sum over times, by user_id and action_detail
  sessions_action_detail_flg_sum <- sessions[,list(flg_sum = sum(flg, na.rm=T)),
                                             by=list(user_id, action_detail)]
  sessions_action_detail_flg_sum <- data.frame(sessions_action_detail_flg_sum)
  names(sessions_action_detail_flg_sum) <- c("id", "feature", "value")
  sessions_action_detail_flg_sum$feature <- paste("action_detail_flg_sum", sessions_action_detail_flg_sum$feature, sep="_")
  n_distinct(sessions_action_detail_flg_sum$feature)
  saveRDS(sessions_action_detail_flg_sum, "cache/sessions_action_detail_flg_sum.RData")
  
  #new feature "sessions_device_type_flg_sum": sum over times, by user_id and device_type
  sessions_device_type_flg_sum <- sessions[,list(flg_sum = sum(flg, na.rm=T)),
                                           by=list(user_id, device_type)]
  sessions_device_type_flg_sum <- data.frame(sessions_device_type_flg_sum)
  names(sessions_device_type_flg_sum) <- c("id", "feature", "value")
  sessions_device_type_flg_sum$feature <- paste("device_type_flg_sum", sessions_device_type_flg_sum$feature, sep="_")
  n_distinct(sessions_device_type_flg_sum$feature) # 14
  saveRDS(sessions_device_type_flg_sum, "cache/sessions_device_type_flg_sum.RData")
  
  #new feature "sessions_action_se_mean": mean over secs_elapsed, by user_id and action
  sessions_action_se_mean <- sessions[,list(secs_elapsed_mean = mean(secs_elapsed, na.rm=T)),
                                      by=list(user_id, action)]
  sessions_action_se_mean <- data.frame(sessions_action_se_mean)
  names(sessions_action_se_mean) <- c("id", "feature", "value")
  sessions_action_se_mean$feature <- paste("action_se_mean", sessions_action_se_mean$feature, sep="_")
  n_distinct(sessions_action_se_mean$feature) #360
  saveRDS(sessions_action_se_mean, "cache/sessions_action_se_mean.RData")
  
  #new feature "sessions_action_type_se_mean":mean over secs_elapsed, by user_id and action_type
  sessions_action_type_se_mean <- sessions[,list(secs_elapsed_mean = mean(secs_elapsed, na.rm=T)),
                                           by=list(user_id, action_type)]
  sessions_action_type_se_mean <- data.frame(sessions_action_type_se_mean)
  names(sessions_action_type_se_mean) <- c("id", "feature", "value")
  sessions_action_type_se_mean$feature <- paste("action_type_se_mean", sessions_action_type_se_mean$feature, sep="_")
  n_distinct(sessions_action_type_se_mean$feature) #11
  saveRDS(sessions_action_type_se_mean, "cache/sessions_action_type_se_mean.RData")
  
  #new feature "sessions_action_detail_se_mean": mean over secs_elapsed, by user_id and action_detail.
  sessions_action_detail_se_mean <- sessions[,list(secs_elapsed_mean = mean(secs_elapsed, na.rm=T)),
                                             by=list(user_id, action_detail)]
  sessions_action_detail_se_mean <- data.frame(sessions_action_detail_se_mean)
  names(sessions_action_detail_se_mean) <- c("id", "feature", "value")
  sessions_action_detail_se_mean$feature <- paste("action_detail_se_mean", sessions_action_detail_se_mean$feature, sep="_")
  n_distinct(sessions_action_detail_se_mean$feature) #156
  saveRDS(sessions_action_detail_se_mean, "cache/sessions_action_detail_se_mean.RData")
  
  #new feature "sessions_device_type_se_mean": mean over secs_elapsed, by user_id and device_type
  sessions_device_type_se_mean <- sessions[,list(secs_elapsed_mean = mean(secs_elapsed, na.rm=T)),
                                           by=list(user_id, device_type)]
  sessions_device_type_se_mean <- data.frame(sessions_device_type_se_mean)
  names(sessions_device_type_se_mean) <- c("id", "feature", "value")
  sessions_device_type_se_mean$feature <- paste("device_type_se_mean", sessions_device_type_se_mean$feature, sep="_")
  n_distinct(sessions_device_type_se_mean$feature) #14
  saveRDS(sessions_device_type_se_mean, "cache/sessions_device_type_se_mean.RData")
  
  #new feature "sessions_action_se_sd": standard deviation over secs_elapsed, by user_id and action
  sessions_action_se_sd <- sessions[,list(secs_elapsed_sd = sd(secs_elapsed, na.rm=T)),
                                    by=list(user_id, action)]
  sessions_action_se_sd <- data.frame(sessions_action_se_sd)
  names(sessions_action_se_sd) <- c("id", "feature", "value")
  sessions_action_se_sd$feature <- paste("action_se_sd", sessions_action_se_sd$feature, sep="_")
  n_distinct(sessions_action_se_sd$feature) #360
  saveRDS(sessions_action_se_sd, "cache/sessions_action_se_sd.RData")
  
  #new feature "sessions_action_type_se_sd": standard deviation over secs_elapsed, by user_id and action_type 
  sessions_action_type_se_sd <- sessions[,list(secs_elapsed_sd = sd(secs_elapsed, na.rm=T)),
                                         by=list(user_id, action_type)]
  sessions_action_type_se_sd <- data.frame(sessions_action_type_se_sd)
  names(sessions_action_type_se_sd) <- c("id", "feature", "value")
  sessions_action_type_se_sd$feature <- paste("action_type_se_sd", sessions_action_type_se_sd$feature, sep="_")
  n_distinct(sessions_action_type_se_sd$feature) #11
  saveRDS(sessions_action_type_se_sd, "cache/sessions_action_type_se_sd.RData")
  
  #new feature "sessions_action_detail_se_sd": standard deviation over secs_elapsed, by user_id and aciton_detail
  sessions_action_detail_se_sd <- sessions[,list(secs_elapsed_sd = sd(secs_elapsed, na.rm=T)),
                                           by=list(user_id, action_detail)]
  sessions_action_detail_se_sd <- data.frame(sessions_action_detail_se_sd)
  names(sessions_action_detail_se_sd) <- c("id", "feature", "value")
  sessions_action_detail_se_sd$feature <- paste("action_detail_se_sd", sessions_action_detail_se_sd$feature, sep="_")
  n_distinct(sessions_action_detail_se_sd$feature) #156
  saveRDS(sessions_action_detail_se_sd, "cache/sessions_action_detail_se_sd.RData")
  
  #new feature "sessions_device_type_se_sd": standard deviation over secs_elapsed, by user_id and device_type
  sessions_device_type_se_sd <- sessions[,list(secs_elapsed_sd = sd(secs_elapsed, na.rm=T)),
                                         by=list(user_id, device_type)]
  sessions_device_type_se_sd <- data.frame(sessions_device_type_se_sd)
  names(sessions_device_type_se_sd) <- c("id", "feature", "value")
  sessions_device_type_se_sd$feature <- paste("device_type_se_sd", sessions_device_type_se_sd$feature, sep="_")
  n_distinct(sessions_device_type_se_sd$feature) #14
  saveRDS(sessions_device_type_se_sd, "cache/sessions_device_type_se_sd.RData")
  
  #new feature "sessions_action_se_wrmean": weight-mean over secs_elapsed, by user_id and action. weight value:reverse order by record for each user
  sessions_action_se_wrmean <- sessions[,list(secs_elapsed_wrmean = weighted.mean(secs_elapsed, w = 1/seq_rev)),
                                        by=list(user_id, action)]
  sessions_action_se_wrmean <- data.frame(sessions_action_se_wrmean)
  names(sessions_action_se_wrmean) <- c("id", "feature", "value")
  sessions_action_se_wrmean$feature <- paste("action_se_wrmean", sessions_action_se_wrmean$feature, sep="_")
  n_distinct(sessions_action_se_wrmean$feature)
  saveRDS(sessions_action_se_wrmean, "cache/sessions_action_se_wrmean.RData")
  
  
  #new feature "sessions_action_type_se_wrmean": weight-mean over secs_elapsed, by user_id and action_type 
  sessions_action_type_se_wrmean <- sessions[,list(secs_elapsed_wrmean = weighted.mean(secs_elapsed, w = 1/seq_rev)),
                                             by=list(user_id, action_type)]
  sessions_action_type_se_wrmean <- data.frame(sessions_action_type_se_wrmean)
  names(sessions_action_type_se_wrmean) <- c("id", "feature", "value")
  sessions_action_type_se_wrmean$feature <- paste("action_type_se_wrmean", sessions_action_type_se_wrmean$feature, sep="_")
  n_distinct(sessions_action_type_se_wrmean$feature)
  saveRDS(sessions_action_type_se_wrmean, "cache/sessions_action_type_se_wrmean.RData")
  
  #new feature "sessions_action_detail_se_wrmean": weight-mean over secs_elapsed, by user_id and action_detail 
  sessions_action_detail_se_wrmean <- sessions[,list(secs_elapsed_wrmean = weighted.mean(secs_elapsed, w = 1/seq_rev)),
                                               by=list(user_id, action_detail)]
  sessions_action_detail_se_wrmean <- data.frame(sessions_action_detail_se_wrmean)
  names(sessions_action_detail_se_wrmean) <- c("id", "feature", "value")
  sessions_action_detail_se_wrmean$feature <- paste("action_detail_se_wrmean", sessions_action_detail_se_wrmean$feature, sep="_")
  n_distinct(sessions_action_detail_se_wrmean$feature)
  saveRDS(sessions_action_detail_se_wrmean, "cache/sessions_action_detail_se_wrmean.RData")
  
  #new feature "sessions_device_type_se_wrmean": weight-mean over secs_elapsed, by user_id and device_type 
  sessions_device_type_se_wrmean <- sessions[,list(secs_elapsed_wrmean = weighted.mean(secs_elapsed, w = 1/seq_rev)),
                                             by=list(user_id, device_type)]
  sessions_device_type_se_wrmean <- data.frame(sessions_device_type_se_wrmean)
  names(sessions_device_type_se_wrmean) <- c("id", "feature", "value")
  sessions_device_type_se_wrmean$feature <- paste("device_type_se_wrmean", sessions_device_type_se_wrmean$feature, sep="_")
  n_distinct(sessions_device_type_se_wrmean$feature)
  saveRDS(sessions_device_type_se_wrmean, "cache/sessions_device_type_se_wrmean.RData")
  
  #new feature "sessions_action_se_wmean": weight-mean over secs_elapsed, by user_id and action, order by user record for each user
  sessions_action_se_wmean <- sessions[,list(secs_elapsed_wmean = weighted.mean(secs_elapsed, w = 1/seq)),
                                       by=list(user_id, action)]
  sessions_action_se_wmean <- data.frame(sessions_action_se_wmean)
  names(sessions_action_se_wmean) <- c("id", "feature", "value")
  sessions_action_se_wmean$feature <- paste("action_se_wmean", sessions_action_se_wmean$feature, sep="_")
  n_distinct(sessions_action_se_wmean$feature)
  saveRDS(sessions_action_se_wmean, "cache/sessions_action_se_wmean.RData")
  
  #new feature "sessions_action_type_se_wmean": weight-mean over secs_elapsed, by user_id and action_type
  sessions_action_type_se_wmean <- sessions[,list(secs_elapsed_wmean = weighted.mean(secs_elapsed, w = 1/seq)),
                                            by=list(user_id, action_type)]
  sessions_action_type_se_wmean <- data.frame(sessions_action_type_se_wmean)
  names(sessions_action_type_se_wmean) <- c("id", "feature", "value")
  sessions_action_type_se_wmean$feature <- paste("action_type_se_wmean", sessions_action_type_se_wmean$feature, sep="_")
  n_distinct(sessions_action_type_se_wmean$feature)
  saveRDS(sessions_action_type_se_wmean, "cache/sessions_action_type_se_wmean.RData")
  
  #new feature "sessions_action_detail_se_wmean": weight-mean over secs_elapsed, by user_id and action_detail
  sessions_action_detail_se_wmean <- sessions[,list(secs_elapsed_wmean = weighted.mean(secs_elapsed, w = 1/seq)),
                                              by=list(user_id, action_detail)]
  sessions_action_detail_se_wmean <- data.frame(sessions_action_detail_se_wmean)
  names(sessions_action_detail_se_wmean) <- c("id", "feature", "value")
  sessions_action_detail_se_wmean$feature <- paste("action_detail_se_wmean", sessions_action_detail_se_wmean$feature, sep="_")
  n_distinct(sessions_action_detail_se_wmean$feature)
  saveRDS(sessions_action_detail_se_wmean, "cache/sessions_action_detail_se_wmean.RData")
  
  #new feature "sessions_device_type_se_wmean": weight-mean over secs_elapsed, by user_id and device_type
  sessions_device_type_se_wmean <- sessions[,list(secs_elapsed_wmean = weighted.mean(secs_elapsed, w = 1/seq)),
                                            by=list(user_id, device_type)]
  sessions_device_type_se_wmean <- data.frame(sessions_device_type_se_wmean)
  names(sessions_device_type_se_wmean) <- c("id", "feature", "value")
  sessions_device_type_se_wmean$feature <- paste("device_type_se_wmean", sessions_device_type_se_wmean$feature, sep="_")
  n_distinct(sessions_device_type_se_wmean$feature)
  saveRDS(sessions_device_type_se_wmean, "cache/sessions_device_type_se_wmean.RData")
}

sessions_action_se_sum <- readRDS("cache/sessions_action_se_sum.RData")
sessions_action_type_se_sum <- readRDS("cache/sessions_action_type_se_sum.RData")
sessions_action_detail_se_sum <- readRDS("cache/sessions_action_detail_se_sum.RData")
sessions_device_type_se_sum <- readRDS("cache/sessions_device_type_se_sum.RData")
sessions_action_flg_sum <- readRDS("cache/sessions_action_flg_sum.RData")
sessions_action_type_flg_sum <- readRDS("cache/sessions_action_type_flg_sum.RData")
sessions_action_detail_flg_sum <- readRDS("cache/sessions_action_detail_flg_sum.RData")
sessions_device_type_flg_sum <- readRDS("cache/sessions_device_type_flg_sum.RData")
sessions_action_se_mean <- readRDS("cache/sessions_action_se_mean.RData")
sessions_action_type_se_mean <- readRDS("cache/sessions_action_type_se_mean.RData")
sessions_action_detail_se_mean <- readRDS("cache/sessions_action_detail_se_mean.RData")
sessions_device_type_se_mean <- readRDS("cache/sessions_device_type_se_mean.RData")
sessions_action_se_sd <- readRDS("cache/sessions_action_se_sd.RData")
sessions_action_type_se_sd <- readRDS("cache/sessions_action_type_se_sd.RData")
sessions_action_detail_se_sd <- readRDS("cache/sessions_action_detail_se_sd.RData")
sessions_device_type_se_sd <- readRDS("cache/sessions_device_type_se_sd.RData")
sessions_action_se_wrmean <- readRDS("cache/sessions_action_se_wrmean.RData")
sessions_action_type_se_wrmean <- readRDS("cache/sessions_action_type_se_wrmean.RData")
sessions_action_detail_se_wrmean <- readRDS("cache/sessions_action_detail_se_wrmean.RData")
sessions_device_type_se_wrmean <- readRDS("cache/sessions_device_type_se_wrmean.RData")
sessions_action_se_wmean <- readRDS("cache/sessions_action_se_wmean.RData")
sessions_action_type_se_wmean <- readRDS("cache/sessions_action_type_se_wmean.RData")
sessions_action_detail_se_wmean <- readRDS("cache/sessions_action_detail_se_wmean.RData")
sessions_device_type_se_wmean <- readRDS("cache/sessions_device_type_se_wmean.RData")


# **************************************
# countries features
# **************************************
countries <- dplyr::mutate(countries,
                           country_language = paste0(country_destination, "_", destination_language))
countries_reshape <- data.frame()
for(i in unique(countries$country_language)){
  # i <- "AU_eng"
  countries_ <- subset(countries, country_language == i)
  countries_$country_language <- NULL
  countries_ <- reshape(countries_,
                        direction='wide',
                        idvar='destination_language',
                        timevar='country_destination')
  countries_reshape <- bind_rows(
    countries_reshape,
    countries_
  )
}

countries_reshape <- countries_reshape %>%
  dplyr::group_by(destination_language) %>%
  dplyr::summarise_each(funs(Sum))

countries_reshape <- dplyr::mutate(countries_reshape,
                                   destination_language = str_sub(destination_language, 1, 2))

df_all_countries_feats <- dplyr::left_join(df_all[c("id", "language")],
                                           countries_reshape,
                                           by = c("language" = "destination_language"))
df_all_countries_feats$language <- NULL
df_all_countries_feats <- melt.data.table(as.data.table(df_all_countries_feats))
df_all_countries_feats <- data.frame(df_all_countries_feats)
names(df_all_countries_feats) <- c("id", "feature", "value")
print("countries feature")
print(n_distinct(df_all_countries_feats$feature))


# **************************************
# age_gender_bkts features
# **************************************
age_gender_bkts <- age_gender_bkts %>%
  dplyr::left_join(., countries, by = "country_destination")

age_gender_bkts <- age_gender_bkts[c("age_bucket", "country_destination", "gender", "population_in_thousands", "year", "destination_language")]

age_gender_bkts <- dplyr::mutate(age_gender_bkts,
                                 country_language = paste0(country_destination, "_", destination_language))
age_gender_bkts_reshape <- data.frame()
for(i in unique(age_gender_bkts$country_language)){
  # i <- "AU_eng"
  age_gender_bkts_ <- subset(age_gender_bkts, country_language == i)
  age_gender_bkts_$country_language <- NULL
  age_gender_bkts_ <- reshape(age_gender_bkts_,
                              direction='wide',
                              idvar=c('destination_language', 'age_bucket', 'gender', 'year'),
                              timevar='country_destination')
  age_gender_bkts_reshape <- bind_rows(
    age_gender_bkts_reshape,
    age_gender_bkts_
  )
}

age_gender_bkts_reshape$year <- NULL
age_gender_bkts_reshape <- age_gender_bkts_reshape %>%
  dplyr::mutate(.,
                gender = toupper(gender),
                language = str_sub(destination_language, 1, 2)
  ) %>%
  dplyr::select(-destination_language) %>%
  dplyr::group_by(age_bucket, gender, language) %>%
  dplyr::summarise_each(funs(Sum))

df_all_age_gender_bkts_feats <- dplyr::left_join(df_all[c("id", "age_bucket", "gender", "language")],
                                                 age_gender_bkts_reshape,
                                                 by = c("age_bucket", "gender", "language"))
df_all_age_gender_bkts_feats$age_bucket <- NULL
df_all_age_gender_bkts_feats$gender <- NULL
df_all_age_gender_bkts_feats$language <- NULL
df_all_age_gender_bkts_feats <- melt.data.table(as.data.table(df_all_age_gender_bkts_feats))
df_all_age_gender_bkts_feats <- data.frame(df_all_age_gender_bkts_feats)
names(df_all_age_gender_bkts_feats) <- c("id", "feature", "value")
print("age_gender_bkts feature")
print(n_distinct(df_all_age_gender_bkts_feats$feature))


# **************************************
# feature binding
# **************************************
df_all_feats <- 
  bind_rows(
    df_all_num_feats,
    df_all_ohe_feats,
    sessions_action_se_sum,
    sessions_action_type_se_sum,
    sessions_action_detail_se_sum,
    sessions_device_type_se_sum,
    sessions_action_flg_sum,
    sessions_action_type_flg_sum,
    sessions_action_detail_flg_sum,
    sessions_device_type_flg_sum,
    # sessions_action_se_mean,
    # sessions_action_type_se_mean,
    # sessions_action_detail_se_mean,
    # sessions_device_type_se_mean,
    # sessions_action_se_sd,
    # sessions_action_type_se_sd,
    # sessions_action_detail_se_sd,
    # sessions_device_type_se_sd,
    # sessions_action_se_wrmean,
    # sessions_action_type_se_wrmean,
    # sessions_action_detail_se_wrmean,
    # sessions_device_type_se_wrmean,
    # sessions_action_se_wmean,
    # sessions_action_type_se_wmean,
    # sessions_action_detail_se_wmean,
    # sessions_device_type_se_wmean,
    df_all_countries_feats,
    df_all_age_gender_bkts_feats
  )
print("feature number")
print(n_distinct(df_all_feats$feature))

saveRDS(labels, paste0("cache/",folder,"/labels.RData"))
saveRDS(sample_submission_NDF, paste0("cache/",folder,"/sample_submission_NDF.RData"))
saveRDS(df_all, paste0("cache/",folder,"/df_all.RData"))
saveRDS(df_all_feats, paste0("cache/",folder,"/df_all_feats.RData"))