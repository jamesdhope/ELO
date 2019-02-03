
#############################################
#											#
#											#
#			Data Import & 					#
#			Environment Config				#
#											#
#############################################

list.of.packages <- c("corrplot","xgboost","mlbench","caret","lubridate","magrittr","tidyverse","dplyr","ggplot2","Matrix","Ckmeans.1d.dp")
repo='http://nbcgib.uesc.br/mirrors/cran/'
install.packages(list.of.packages, repo = repo)
lapply(list.of.packages, require, character.only = TRUE)

library(data.table)
library(lubridate)
library(scales)
library(ggplot2)
library(Matrix)
library(dplyr)
library(gridExtra)
library(corrplot)

test <- read_csv("~/desktop/capstone_2/test.csv")
train <- read_csv("~/desktop/capstone_2/train.csv")

head(test) 
head(train) 

#############################################
#											#
#											#
#			Analysis & Visualisation		#
#											#
#											#
#############################################

train %>% 
  bind_rows(test) %>% 
  mutate(set = factor(if_else(is.na(target), "Test", "Train")),
         first_active_month = ymd(first_active_month, truncated = 1)) %>% 
  ggplot(aes(x = first_active_month, fill = set)) +
  geom_bar() +
  theme_minimal()

 train %>% 
  bind_rows(test) %>% 
  mutate(set = factor(if_else(is.na(target), "Test", "Train")),
         first_active_month = ymd(first_active_month, truncated = 1)) %>% 
  ggplot(aes(x = first_active_month, fill = set)) +
  geom_bar() +
  theme_minimal()

train %>% 
  bind_rows(test) %>% 
  mutate(set = factor(if_else(is.na(target), "Test", "Train"))) %>% 
  select(-first_active_month, -card_id, -target) %>% 
  gather(key = "feature", value = "value", -set) %>% 
  mutate(value = factor(value)) %>% 
  ggplot(aes(value, fill = set)) +
  geom_bar(aes(y=..prop.., group = 1)) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(set ~ feature, scales = "free") +
  theme_minimal()

 train %>% 
  mutate(feature_1 = factor(feature_1),
         feature_2 = factor(feature_2),
         feature_2 = factor(feature_2)) %>% 
  select(-first_active_month, -card_id) %>% 
  model.matrix(~.-1, .) %>% 
  cor(method = "spearman") %>%
  corrplot(type="lower", method = "number", tl.col = "black", diag=FALSE, tl.cex = 0.9, number.cex = 0.9)

sample_submission <- read_csv("~/desktop/capstone_2/sample_submission.csv")
merchants <- read_csv("~/desktop/capstone_2/merchants.csv")
historical_transactions <- read_csv("~/desktop/capstone_2/historical_transactions.csv")
new_merchant_transactions <- read_csv("~/desktop/capstone_2/new_merchant_transactions.csv")

head(sample_submission) 
head(historical_transactions) 
head(merchants) 
head(new_merchant_transactions) 

p1 <- historical_transactions %>%
  sample_n(6e6) %>% 
  mutate(date = date(purchase_date)) %>% 
  ggplot(aes(x = date)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Historical")
p2 <- new_merchant_transactions %>%
  mutate(date = date(purchase_date)) %>% 
  ggplot(aes(x = date)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New")
grid.arrange(p1, p2, ncol=2)

p1 <- historical_transactions %>% 
  sample_n(6e6) %>% 
  mutate(hour = hour(purchase_date)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  ggtitle("Historical")
p2 <- new_merchant_transactions %>% 
  mutate(hour = hour(purchase_date)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  ggtitle("New")

grid.arrange(p1, p2, ncol=2)

p1 <- historical_transactions %>%
  sample_n(6e6) %>% 
  select(authorized_flag, category_1, category_2, category_3) %>% 
  mutate(category_2 = as.character(category_2)) %>% 
  gather(key = "feature", value = "value") %>% 
  mutate(value = factor(value)) %>% 
  ggplot(aes(value, fill = feature)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~ feature, scales = "free") +
  theme_minimal() +
  ggtitle("Historical") +
  theme(legend.position = "none")
p2 <- new_merchant_transactions %>%
  select(authorized_flag, category_1, category_2, category_3) %>% 
  mutate(category_2 = as.character(category_2)) %>% 
  gather(key = "feature", value = "value") %>% 
  mutate(value = factor(value)) %>% 
  ggplot(aes(value, fill = feature)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~ feature, scales = "free") +
  theme_minimal() +
  ggtitle("New") +
  theme(legend.position = "none")
grid.arrange(p1, p2, ncol = 2)

#############################################
#											#
#											#
#			Data Processing					#
#											#
#											#
#############################################

train_data <-read_csv("~/desktop/capstone_2/train.csv")
test_data <-read_csv("~/desktop/capstone_2/test.csv")
htrans <- read_csv("~/desktop/capstone_2/historical_transactions.csv")
merchants <- read_csv("~/desktop/capstone_2/merchants.csv")
ntrans <- read_csv("~/desktop/capstone_2/new_merchant_transactions.csv")

htrans_auth <- htrans %>% 
  filter(authorized_flag == "Y") %>% 
  select(-authorized_flag) %>% 
  rename(card = card_id)

htrans <- htrans %>% 
  filter(authorized_flag == "N") %>% 
  select(-authorized_flag) %>% 
  rename(card = card_id)

ntrans %<>% 
  left_join(merchants, by = "merchant_id", suffix = c("", "_mer")) %>%
  select(-authorized_flag) %>% 
  rename(card = card_id)  

rm(merchants); invisible(gc())  

for (tx in c("htrans_auth", "htrans", "ntrans")) {

  ohe <- paste0("ohe_", tx)
  assign(ohe,
         get(tx) %>%
           select(starts_with("category"), starts_with("most_recent")) %>% 
           mutate_all(factor) %>% 
           model.matrix.lm(~ . - 1, ., na.action = NULL) %>% 
           as_tibble())
  
  fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))
  
  sum_tx <- paste0("sum_", tx) 
  assign(sum_tx, 
         get(tx) %>%
           select(-starts_with("category"), -starts_with("most_recent"), -contains("_id")) %>% 
           add_count(card) %>%
           group_by(card) %>%
           mutate(date_diff = as.integer(diff(range(purchase_date))),
                  prop = n() / sum(n)) %>% 
           ungroup() %>% 
           mutate(year = year(purchase_date),
                  month = month(purchase_date),
                  day = day(purchase_date),
                  hour = hour(purchase_date),
                  month_diff = as.integer(ymd("2018-12-01") - date(purchase_date)) / 30 + month_lag) %>% 
           select(-purchase_date) %>% 
           bind_cols(get(ohe)) %>% 
           group_by(card) %>%
           summarise_all(fn))
           
  rm(list = c(ohe, tx, "fn", "ohe", "sum_tx"))
  gc()
}

# Joining dataset
tr <-train_data 
te <- test_data

tri <- 1:nrow(tr)
y <- tr$target

tr_te <- tr %>% 
  select(-target) %>% 
  bind_rows(te) %>%
  rename(card = card_id) %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         date_diff = as.integer(ymd("2018-02-01") - first_active_month),
         weekend = as.integer(wday(first_active_month) %in% c(1, 7))) %>% 
  select(-first_active_month) %>% 
  left_join(sum_htrans_auth, by = "card") %>% 
  left_join(sum_htrans, by = "card") %>% 
  left_join(sum_ntrans, by = "card") %>% 
  select(-card) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  select_if(~ n_distinct(.x) > 1) %>% 
  data.matrix()
  
cols <- colnames(tr_te)  
#rm(tr, te, sum_htrans_auth, sum_htrans, sum_ntrans, tx)
invisible(gc())

#############################################
#											#
#											#
#			Model Buildling					#
#											#
#											#
#############################################

#preparing Data to apply XGB
install.packages("xgboost", repos='http://nbcgib.uesc.br/mirrors/cran/')
library(xgboost)

val <- caret::createDataPartition(y, p = 0.2, list = FALSE)
dtrain <- xgb.DMatrix(data = tr_te[tri, ][-val, ], label = y[-val])
dval <- xgb.DMatrix(data = tr_te[tri, ][val, ], label = y[val])
dtest <- xgb.DMatrix(data = tr_te[-tri, ])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

p <- list(objective = "reg:linear",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 4,
          eta = 0.02,
          max_depth = 7,
          min_child_weight = 100,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.8,
          colsample_bylevel = 0.8,
          alpha = 0,
          lambda = 0.1)

set.seed(0)
m_xgb <- xgb.train(p, dtrain, 600, list(val = dval), print_every_n = 50, early_stopping_rounds = 200, nfold=4)

#############################################
#											#
#											#
#			Model Prediction				#
#											#
#											#
#############################################

target <- predict(m_xgb,dtest)
best_score <- round(m_xgb$best_score, 5)

best_score

read_csv("~/desktop/capstone_2/sample_submission.csv") %>%  
  mutate(target = predict(m_xgb, dtest)) %>%
  write_csv(paste0("submission", round(m_xgb$best_score, 5), ".csv"))



