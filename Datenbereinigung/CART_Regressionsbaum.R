# https://rpubs.com/camguild/803096
# https://uc-r.github.io/regression_trees
# https://quantdev.ssri.psu.edu/tutorials/introduction-classification-regression-trees


######################### Lesen der RData Datei ##########################

load("spotify_songs_cleaned_with_trans.RData")
load("spotify_songs_cleaned_with_trans_optima.RData")
load("spotify_songs_cleaned_without_trans.RData")


str(spotify_songs_cleaned_with_trans)
str(spotify_songs_cleaned_with_trans_optima)
str(spotify_songs_cleaned_without_trans)


library("rpart")
library("rpart.plot")
#library("rsample")     # data splitting 
library("dplyr")       # data wrangling
library("ipred")       # bagging
library("caret")       # bagging
library("Metrics")

#########################################################

# spotify_songs_cleaned_with_trans_optima (working)

#########################################################

# Normalisierung der numerischen Werte

spotify_songs_cleaned_with_trans_scaled <- spotify_songs_cleaned_with_trans_optima

streams_. <- spotify_songs_cleaned_with_trans_optima$streams

spotify_songs_cleaned_with_trans_scaled$streams <- NULL

spotify_songs_cleaned_with_trans_scaled <- as.data.frame(lapply(spotify_songs_cleaned_with_trans_optima,
                                           function(x){
                                             if(is.numeric(x)) scale(x) else x
                                           }))
spotify_songs_cleaned_with_trans_scaled$streams <- streams_.


# Aufteilung der Daten in Trainings- und Testdatensatz
set.seed(123) # Setzt einen Seed für reproduzierbare Ergebnisse
index <- sample(1:nrow(spotify_songs_cleaned_with_trans_scaled), size = floor(0.8 * nrow(spotify_songs_cleaned_with_trans_scaled)))
trainData <- spotify_songs_cleaned_with_trans_scaled[index, ]
testData <- spotify_songs_cleaned_with_trans_scaled[-index, ]


base_spotify_tree <- rpart(
  formula = streams ~ .,
  data    = trainData,
  method  = "anova"
)



base_spotify_tree

# n= 759 
# 
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 759 859.17210 19.52521  
# 2) in_spotify_playlists_log< 8.057977 467 263.69280 18.90180  
# 4) in_spotify_playlists_log< 7.350192 308 145.71400 18.63891  
# 8) years_since_release_boxcox< -2.103777 116  50.68277 18.24715  
# 16) released_month=May,June,July 56  19.20017 17.87023 *
#   17) released_month=January,February,March,April 60  16.10159 18.59894 *
#     9) years_since_release_boxcox>=-2.103777 192  66.47163 18.87560  
# 18) in_spotify_charts< 0.50005 106  35.05817 18.64764 *
#   19) in_spotify_charts>=0.50005 86  19.11535 19.15658 *
#   5) in_spotify_playlists_log>=7.350192 159  55.46143 19.41103  
# 10) in_spotify_charts< 0.50005 86  24.63235 19.18721 *
#   11) in_spotify_charts>=0.50005 73  21.44535 19.67471 *
#   3) in_spotify_playlists_log>=8.057977 292 123.71870 20.52223  
# 6) in_spotify_playlists_log< 9.389113 198  53.90232 20.22491  
# 12) in_spotify_playlists_log< 8.711113 109  24.50687 20.02844 *
#   13) in_spotify_playlists_log>=8.711113 89  20.03550 20.46552 *
#   7) in_spotify_playlists_log>=9.389113 94  15.44412 21.14851 *



rpart.plot(base_spotify_tree)

plotcp(base_spotify_tree)


full_spotify_tree <- rpart(
  formula = streams ~ .,
  data    = trainData,
  method  = "anova", 
  control = list(cp = 0, xval = 10)
)

plotcp(full_spotify_tree)
#abline(v = 12, lty = "dashed")


full_spotify_tree$cptable
# CP nsplit  rel error    xerror       xstd
# 1  0.5490873773      0 1.00000000 1.0029201 0.04459681
# 2  0.0727646398      1 0.45091262 0.4845721 0.02461913
# 3  0.0632845292      2 0.37814798 0.4026154 0.02097691
# 4  0.0332408605      3 0.31486345 0.3540625 0.01948731
# 5  0.0179021182      4 0.28162259 0.3178494 0.01762835
# 6  0.0143139052      5 0.26372047 0.2987042 0.01626070
# 7  0.0109218266      6 0.24940657 0.2875180 0.01639353
# 8  0.0108941510      7 0.23848474 0.2894957 0.01665202
# 9  0.0095989714      8 0.22759059 0.2838396 0.01647537
# 10 0.0083350931      9 0.21799162 0.2728423 0.01622995
# 11 0.0072345635     10 0.20965653 0.2638677 0.01566409
# 12 0.0062121959     11 0.20242196 0.2615996 0.01668397
# 13 0.0060322131     12 0.19620977 0.2502268 0.01612834
# 14 0.0057148872     13 0.19017756 0.2448675 0.01606250
# 15 0.0056381796     14 0.18446267 0.2412955 0.01600995
# 16 0.0055199003     15 0.17882449 0.2423167 0.01603001
# 17 0.0053192544     16 0.17330459 0.2409810 0.01573354
# 18 0.0048217589     17 0.16798533 0.2390685 0.01567152
# 19 0.0041703361     18 0.16316357 0.2366523 0.01557155
# 20 0.0041346664     19 0.15899324 0.2347716 0.01569985
# 21 0.0038922038     20 0.15485857 0.2353703 0.01575756
# 22 0.0038457942     22 0.14707416 0.2332243 0.01559191
# 23 0.0037791354     23 0.14322837 0.2329680 0.01558964
# 24 0.0032827430     24 0.13944923 0.2328521 0.01559839
# 25 0.0031079227     25 0.13616649 0.2308413 0.01564101
# 26 0.0026221787     26 0.13305857 0.2328043 0.01601368
# 27 0.0025671745     27 0.13043639 0.2331551 0.01595950
# 28 0.0025429809     28 0.12786922 0.2310848 0.01590093
# 29 0.0024768318     29 0.12532624 0.2319178 0.01590751
# 30 0.0024749267     30 0.12284940 0.2313251 0.01590936
# 31 0.0023301008     31 0.12037448 0.2319830 0.01589855
# 32 0.0023085820     32 0.11804438 0.2324238 0.01591177
# 33 0.0022546606     33 0.11573579 0.2322014 0.01593868
# 34 0.0022427033     34 0.11348113 0.2300271 0.01615726
# 35 0.0020456804     35 0.11123843 0.2271065 0.01607033
# 36 0.0020402879     36 0.10919275 0.2260753 0.01607808
# 37 0.0016870728     37 0.10715246 0.2272454 0.01614273
# 38 0.0014089808     38 0.10546539 0.2306622 0.01639211
# 39 0.0013079762     39 0.10405641 0.2308117 0.01641601
# 40 0.0012940995     41 0.10144046 0.2320461 0.01645666
# 41 0.0012771344     42 0.10014636 0.2325449 0.01675314
# 42 0.0011302513     43 0.09886922 0.2359489 0.01680732
# 43 0.0011299965     44 0.09773897 0.2358665 0.01683879
# 44 0.0010806659     45 0.09660897 0.2358665 0.01683879
# 45 0.0009040994     46 0.09552831 0.2379136 0.01686150
# 46 0.0008354502     47 0.09462421 0.2369778 0.01663663
# 47 0.0008334845     48 0.09378876 0.2383392 0.01671271
# 48 0.0007916329     49 0.09295527 0.2380171 0.01670862
# 49 0.0006918552     50 0.09216364 0.2389147 0.01664936
# 50 0.0006516905     51 0.09147179 0.2406101 0.01667093
# 51 0.0006471171     52 0.09082010 0.2407040 0.01668062
# 52 0.0006327495     53 0.09017298 0.2407040 0.01668062
# 53 0.0005470024     54 0.08954023 0.2410544 0.01668760
# 54 0.0004762586     55 0.08899323 0.2403825 0.01668308
# 55 0.0004216234     56 0.08851697 0.2410360 0.01667870
# 56 0.0003575014     57 0.08809534 0.2408363 0.01666137
# 57 0.0003530693     58 0.08773784 0.2409027 0.01666147
# 58 0.0003434871     59 0.08738477 0.2408518 0.01666232
# 59 0.0000000000     60 0.08704129 0.2410111 0.01666820



# grid search for Hyperparameter

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

head(hyper_grid)
##   minsplit maxdepth
## 1        5        8
## 2        6        8
## 3        7        8
## 4        8        8
## 5        9        8
## 6       10        8

# total number of combinations
nrow(hyper_grid)
## [1] 128

spotify_trees <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  spotify_trees[[i]] <- rpart(
    formula = streams ~ .,
    data    = trainData,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(spotify_trees, get_cp),
    error = purrr::map_dbl(spotify_trees, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)
# 
#    minsplit maxdepth         cp     error
# 1        8       13 0.01000000 0.2644527
# 2        7       11 0.01000000 0.2658812
# 3       10       15 0.01000000 0.2673479
# 4       10       10 0.01089415 0.2693466
# 5       16        8 0.01000000 0.2701820


optimal_tree <- rpart(
  formula = streams ~ .,
  data    = trainData,
  method  = "anova",
  control = list(minsplit = 8, maxdepth = 13, cp = 0.01)
)

rpart.plot(optimal_tree)

pred <- predict(optimal_tree, newdata = testData)
rmse(testData$streams, pred)


log_pred <- pred 
log_actual <- testData$streams  

# Rücktransformation
pred <- exp(log_pred)
actual <- exp(log_actual)

# Berechnung des RMSE
mse <- mean((pred - actual)^2)
rmse <- sqrt(mse)

print(paste("RMSE auf der Originalskala:", rmse))






####################################################################

#Bagging


######################################################################

# make bootstrapping reproducible
set.seed(123)

# train bagged model
bagged_spotify_tree <- bagging(
  formula = streams ~.,
  data    = trainData,
  coob    = TRUE
)

bagged_spotify_tree

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(
  streams ~ .,
  data = trainData,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv


# plot most important variables
plot(varImp(bagged_cv), 20)


pred <- predict(bagged_cv, testData)
RMSE(pred, testData$streams)


#############################################################

#           V2


################################################################

base_spotify_tree <- rpart(
  formula = streams ~ .,
  control = rpart.control(cp = 0),
  data = spotify_songs_cleaned_with_trans_scaled)



pred <- predict(base_spotify_tree, spotify_songs_cleaned_with_trans_scaled)


residuen <- spotify_songs_cleaned_with_trans_scaled$streams - pred


hist(residuen, main = "Histogramm der Residuen", xlab = "Residuen")
plot(spotify_songs_cleaned_with_trans_scaled$streams, residuen, main = "Residuen vs. Beobachtete Werte", xlab = "Beobachtete Werte", ylab = "Residuen")
abline(h = 0, col = "red")


summary(residuen)

ausreisser_index <- which(abs(residuen) > (mean(residuen) + 2 * sd(residuen)))


spotify_songs_cleaned_with_trans_scaled <- spotify_songs_cleaned_with_trans_scaled[-ausreisser_index, ]


rpart.plot(base_spotify_tree)

base_spotify_tree$cptable[, c("CP", "nsplit", "rel error")]

base_spotify_tree$cptable

plotcp(base_spotify_tree)


set.seed(123)
index <- sample(1:nrow(spotify_songs_cleaned_with_trans_scaled), size = floor(0.8 * nrow(spotify_songs_cleaned_with_trans_scaled)))
trainData <- spotify_songs_cleaned_with_trans_scaled[index, ]
testData <- spotify_songs_cleaned_with_trans_scaled[-index, ]

spotify_tree_training <- rpart(
  formula = streams ~ .,
  control = rpart.control(cp = 0),
  data = trainData)

plotcp(spotify_tree_training)
# abline(v = 12, lty = "dashed")

cp_table <- as.data.frame(spotify_tree_training$cptable)
best_cp <- cp_table$CP[which.min(cp_table$xerror)]


spotify_tree_pruned <- prune(spotify_tree_training, cp = best_cp)
rpart.plot(spotify_tree_pruned)

pred <- predict(spotify_tree_training, testData)
RMSE(pred, testData$streams)

# Rücktransformation

log_pred <- pred  
log_actual <- testData$streams  


pred <- exp(log_pred)
actual <- exp(log_actual)

# Berechnung des RMSE
mse <- mean((pred - actual)^2)
rmse <- sqrt(mse)

print(paste("RMSE auf der Originalskala:", rmse))


residuen <- actual - pred


hist(residuen, main = "Histogramm der Residuen", xlab = "Residuen")
plot(actual, residuen, main = "Residuen vs. Beobachtete Werte", xlab = "Beobachtete Werte", ylab = "Residuen")
abline(h = 0, col = "red")


summary(residuen)

ausreisser_index <- which(abs(residuen) > (mean(residuen) + 2 * sd(residuen)))





