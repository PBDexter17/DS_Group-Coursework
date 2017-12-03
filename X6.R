# **************************************
# make dataset for predict country_destination
# **************************************
#get the un null value people.
X6_all <- subset(df_all_feats, is.na(value)==F)
#add new colunm feature_name the content is same as feature
X6_all$feature_name <- X6_all$feature
#get the level of the feature and tranfer them into number and replace the origin feature
#e.g. the level of age_cln is 1081
X6_all$feature <- as.numeric(as.factor(X6_all$feature))

#select the user whose date_account_created day is in "201401", "201402", "201403" "201404", "201405", "201406"
#from df_all and get the subset of these id in X6_all
X6 <- subset(X6_all, id %in% subset(df_all, dac_yearmonth %in% c("20141", "20142", "20143",
                                                                 "20144", "20145", "20146"))$id)

#select the user whose date_account_created day is not in "201401", "201402", "201403" "201404", "201405", "201406"
#from df_all and get the subset of these id in X6_all as test data
X6_test <- subset(X6_all, id %in% subset(df_all, dac_yearmonth %nin% c("20141", "20142", "20143",
                                                                       "20144", "20145", "20146"))$id)
#reconstruct the X6_all
X6_all <- rbind(X6, X6_test)

#select the "feature", "feature_name" and get the un duplicated feature
X6_all_feature <- X6_all[!duplicated(X6_all$feature), c("feature", "feature_name")]
#order theX6_all_feature by feature index
X6_all_feature <- X6_all_feature[order(X6_all_feature$feature),]

#create the new colunm to store the numeric level of id in training set?
X6$id_num <- as.numeric(as.factor(X6$id))
#create the new colunm to store the numeric level of id in testing set?
X6_test$id_num <- as.numeric(as.factor(X6_test$id))

#get un repeat id !! distinct(id_num) should be distinct(id,id_num)
X6_id <- X6 %>% distinct(id,id_num)
#create data frame
X6_id <- data.frame(id = X6_id$id, id_num = X6_id$id_num)
#join the label to the X6_id and the labels is from preprocessing
X6_id <- dplyr::left_join(X6_id, labels, by = "id")

X6_id <- X6_id %>%
  dplyr::mutate(
    #recode is from dplyr package, just like the switch function case
    #change the "'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6,;'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11"
    #to 'NDF'=0, 'US'=1, 'other'=2, 'FR'=3, 'CA'=4, 'GB'=5, 'ES'=6, 'IT'=7, 'PT'=8, 'NL'=9, 'DE'=10, 'AU'=11
    country_destination_num = recode(country_destination,'NDF'=0, 'US'=1, 'other'=2, 'FR'=3, 'CA'=4, 'GB'=5, 'ES'=6, 'IT'=7, 'PT'=8, 'NL'=9, 'DE'=10, 'AU'=11)
  ) %>%
  #arrange is a function from dplyr package, arrange(.data,...) defult is sorting in increse
  #you can use arrange(.data,desc(...)) to sort it in descending order.
  dplyr::arrange(id_num)
y6 <- X6_id$country_destination_num

#get un repeat id !! distinct(id_num) should be distinct(id,id_num) in test data
X6_test_id <- X6_test %>% distinct(id,id_num)
#create data frame
X6_test_id <- data.frame(id = X6_test_id$id, id_num = X6_test_id$id_num)
#arrange the id num
X6_test_id <- X6_test_id %>%
  dplyr::arrange(id_num)
#create test prediction as NA
y6_test <- rep(NA, nrow(X6_test_id))

#The intersection of two sets is the material that they have in common:
#setA<-c("a", "b", "c", "d", "e")
#setB<-c("d", "e", "f", "g")
#the intersect(setA,setB) is "d" and "e"
common_feature <- dplyr::intersect(unique(X6$feature), unique(X6_test$feature))

#delete the row which have Na value. na.omit is from stats package
X6 <- na.omit(X6)
#get the sub set that feature is in common feature
X6 <- subset(X6, feature %in% common_feature)
#sparseMatrix is from matrix package 
# i represent the row and j represent the colunm and x represent the value 
X6_sp <- sparseMatrix(i = X6$id_num,
                      j = X6$feature,
                      x = X6$value)
#check the dimension of X6 spreseMatrix
dim(X6_sp)

#do the same thing to the test set
X6_test <- na.omit(X6_test)
X6_test <- subset(X6_test, feature %in% common_feature)
X6_test_sp <- sparseMatrix(i = X6_test$id_num,
                           j = X6_test$feature,
                           x = X6_test$value)
#check the dimension
dim(X6_test_sp)

#Contruct xgb.DMatrix object from dense matrix, sparse matrix or local file 
#Missing is only used when input is dense matrix, pick a float value that represents missing value. 
#Sometime a data use 0 or other extreme value to represents missing values.
dX6 <- xgb.DMatrix(X6_sp, label = y6, missing = -99999)
dX6_test <- xgb.DMatrix(X6_test_sp, missing = -99999)

# **************************************
# Predict country_destination
# **************************************
#set fold as 10
Folds <- 10
#createFolds splits the data into k groups while createTimeSlices creates cross-validation split for series data.
#create 10 folder cross vlidation
X6_cv <- createFolds(1:nrow(X6_sp), k = Folds)
#create two data frame
X6_stack <- data.frame()
X6_test_stack <- data.frame()

for(i in 1:Folds){ 
  # i <- 1
  #set seed is a function that set the start point of generate the random number
  #X[n+1] = (a * X[n] + c) mod m is a common function to generate the random number
  #and if X[0] is the seed  then it was 0, 3, 1, 4, 2, 0, ...
  # if X[1] is the seed then it was 1, 4, 2, 0, 3, 1, ... and so on
  set.seed(123 * i)
  #create the cross validation folder and the original folder
  #the data
  X6_id_ <- X6_id[-X6_cv[[i]], ]
  X6_fold_id_ <- X6_id[X6_cv[[i]], ]
  #create the cross validation folder and the original folder
  #the sprashmatrix
  X6_sp_ <- X6_sp[-X6_cv[[i]], ]
  X6_fold_sp_ <- X6_sp[X6_cv[[i]], ]
  y6_ <- y6[-X6_cv[[i]]]
  # y6_fold_ <- y6[X6_cv[[i]]]
  
  dX6_ <- xgb.DMatrix(X6_sp_, label = y6_)
  dX6_fold_ <- xgb.DMatrix(X6_fold_sp_)
  
  param <- list("objective" = "multi:softprob",
                "eval_metric" = "mlogloss",
                "num_class" = n_distinct(y6),
                "eta" = 0.01,
                "max_depth" = 6,
                "subsample" = 1.0,
                "colsample_bytree" = 0.3,
                # "lambda" = 1.0,
                "alpha" = 1.0,
                # "min_child_weight" = 6,
                # "gamma" = 10,
                "nthread" = 24)
  
  if (i == 1){
    # Run Cross Valication
    cv.nround = 3000
    bst.cv = xgb.cv(param=param,
                    data = dX6_, 
                    nfold = Folds,
                    nrounds=cv.nround,
                    early.stop.round = 10)
    saveRDS(bst.cv, paste0("cache/",folder,"/X6_bst_cv.RData"))
  }
  
  # train xgboost
  xgb <- xgboost(data = dX6_, 
                 param=param,
                 nround = which.min(bst.cv$test.mlogloss.mean)
  )
  
  # predict values in test set
  y6_fold_ <- predict(xgb, dX6_fold_)
  y6_fold_mat <- matrix(y6_fold_, nrow=nrow(X6_fold_sp_), ncol=n_distinct(y6_), byrow=T)
  y6_fold_df <- as.data.frame(y6_fold_mat)
  country_destination_label <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
  names(y6_fold_df) <- paste0("pred3_", country_destination_label)
  y6_fold_df <- mutate(y6_fold_df,
                       id = unique(X6_fold_id_$id))
  y6_fold_df <- y6_fold_df[c("id", paste0("pred3_", country_destination_label))]
  X6_stack <- bind_rows(X6_stack,
                        y6_fold_df)
  
  y6_test_ <- predict(xgb, dX6_test)
  y6_test_mat <- matrix(y6_test_, nrow=nrow(X6_test_sp), ncol=n_distinct(y6_), byrow=T)
  y6_test_df <- as.data.frame(y6_test_mat)
  names(y6_test_df) <- paste0("pred3_", country_destination_label)
  y6_test_df <- mutate(y6_test_df,
                       id = unique(X6_test_id$id))
  y6_test_df <- y6_test_df[c("id", paste0("pred3_", country_destination_label))]
  X6_test_stack <- bind_rows(X6_test_stack,
                             y6_test_df)
}
X6_test_stack <- X6_test_stack %>%
  group_by(id) %>%
  summarise_each(funs(mean))

X6_stack <- melt.data.table(as.data.table(X6_stack))
X6_stack <- data.frame(X6_stack)
names(X6_stack) <- c("id", "feature", "value")

X6_test_stack <- melt.data.table(as.data.table(X6_test_stack))
X6_test_stack <- data.frame(X6_test_stack)
names(X6_test_stack) <- c("id", "feature", "value")

saveRDS(X6_stack, paste0("cache/",folder,"/X6_stack.RData"))
saveRDS(X6_test_stack, paste0("cache/",folder,"/X6_test_stack.RData"))
gc()
