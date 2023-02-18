## code to prepare all datasets in this package
#initial_seed <- as.integer(Sys.time()) # 1676258013
seed <- 58013

load("C:/Users/rr220803/Documents/Work/Project/2023/HOS - 2023/D2C Model Phase 1/Report/Markdown/V2_non_premium/num_Year_Home_Built/ObjectsforWordReport.Rdata")

# head(preds)
# head(testing_frame)
# dim(testing_frame)
# sum(as.numeric(as.character(testing_frame$y)))/dim(testing_frame)[1]
# head(ImportantVariables)
# dim(ImportantVariables)
# head(IV_WOE_Table_All2)
# dim(IV_WOE_Table_All2)

testing_frame <- cbind(testing_frame, preds)

set.seed(seed = seed)
testing_frame <- testing_frame %>%
  select(y, Age, `Population Density`, glm, xgboost) %>%
  group_by(y) %>%
  slice_sample(prop = 0.005) %>%
  ungroup() %>%
  as.data.frame()


IV_WOE_Table <- IV_WOE_Table_All2 %>%
  filter(Variable %in% ImportantVariables$Variable)


usethis::use_data(testing_frame, overwrite = TRUE)
usethis::use_data(ImportantVariables, overwrite = TRUE)
usethis::use_data(IV_WOE_Table, overwrite = TRUE)
