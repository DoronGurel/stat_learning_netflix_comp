########### Training data (rankings only, no dates):
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_ratings_all.dat")
X.tr = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_y_rating.dat")
y.tr = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_dates_all.dat")
x.da.tr = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_y_date.dat")
y.da.tr = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/test_dates_all.dat")
y.da.te = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/test_ratings_all.dat")
X.te = read.table (con)

con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/movie_titles.txt")
titles = read.table(con,sep=",")
movie_names = as.character(titles[,2])
release_dates = as.numeric(titles[,1])


########### Divide training data into training and validation
n = dim(X.tr)[1]
nva = 2000
ntr = n-nva
va.id = sample (n,nva) # choose 2000 points for validation
#trtr = data.frame (X = X.tr[-va.id,], yda=y.da.tr[-va.id,], y=y.tr[-va.id,]) # include dates
trtr = data.frame(X = X.tr[-va.id,],y=y.tr[-va.id,])
#va = data.frame (X = X.tr[va.id,], yda=y.da.tr[va.id,], y=y.tr[va.id,]) #include dates
va = data.frame (X = X.tr[va.id,],y=y.tr[va.id,])


# fill missing values with column mean
X.tr_center <- apply(X.tr,2,function(x) mean(x[x!=0]))
X.tr_full<-X.tr
for(c in 1:ncol(X.tr)){
  X.tr_full[,c][X.tr_full[,c] == 0] <- X.tr_center[c]
}

# do PCA and train linear regression on PCA
rmse = c()
train.pca = prcomp(X.tr_full[-va.id,], center = TRUE,scale. = TRUE)
va.pca <- scale(X.tr_full[va.id,], center= train.pca$center)
va.pca <- va.pca %*% train.pca$rotation
for(k in 1:50){
  df.trtr.pca = data.frame(X = train.pca$x[,1:k],Y = y.tr[-va.id,])
  pca_lin_mod = lm(Y~., data = df.trtr.pca)
  
  df.va.pca = data.frame(X = va.pca[,1:k],Y = y.tr[va.id,])
  
  lin.pred = predict (pca_lin_mod, newdata=df.va.pca)
  rmse = append(rmse,sqrt(mean((df.va.pca$Y-lin.pred)^2)))
}

plot(rmse)
min(rmse)

# do PCA and train linear regression on PCA -  adding columns by correlation
rmse = c()
train.pca = prcomp(X.tr_full[-va.id,], center = TRUE,scale. = TRUE)
df.trtr.pca = data.frame(X = train.pca$x ,Y = y.tr[-va.id,])
add_to_model = which.max(apply(df.trtr.pca[,-100],2,function(x) abs(cor(x,df.trtr.pca['Y']))))
model_params = as.vector(add_to_model)
new_train = df.trtr.pca[,c(100, model_params)]
model = lm(Y~.,data = new_train)

va.pca <- scale(X.tr_full[va.id,], center= train.pca$center)
va.pca <- va.pca %*% train.pca$rotation
df.va.pca <- data.frame(X = va.pca ,Y = y.tr[va.id,])
lin.pred = predict (model, newdata=df.va.pca)
rmse = append(rmse,sqrt(mean((df.va.pca$Y-lin.pred)^2)) )
for(i in 1:99){
  add_to_model = as.numeric(substr(names(which.max(apply(df.trtr.pca[,-c(100,model_params)],2,function(x) abs(cor(x,model$residuals))))),5,10))
  model_params = as.vector(append(model_params,add_to_model))
  new_train = df.trtr.pca[,c(100, model_params)]
  model = lm(Y~.,data = new_train)
  
  lin.pred = predict (model, newdata=df.va.pca)
  rmse = append(rmse,sqrt(mean((df.va.pca$Y-lin.pred)^2)) )
}

plot(rmse)
min(rmse)
which.min(rmse)



# do PCA and train linear regression on PCA -  adding columns by correlation - indicator(is_early)
rmse = c()
is_early_tr = 1997 + y.da.tr[,1]/365 - 2000 <= 3

train.pca = prcomp(X.tr_full[-va.id,][is_early[-va.id],], center = TRUE,scale. = TRUE)
df.trtr.pca = data.frame(X = train.pca$x ,Y = y.tr[-va.id,][is_early[-va.id]])
add_to_model = which.max(apply(df.trtr.pca[,-100],2,function(x) abs(cor(x,df.trtr.pca['Y']))))
model_params = as.vector(add_to_model)
new_train = df.trtr.pca[,c(100, model_params)]
model = lm(Y~.,data = new_train)

va.pca <- scale(X.tr_full[va.id,][is_early[va.id],], center= train.pca$center)
va.pca <- va.pca %*% train.pca$rotation
df.va.pca <- data.frame(X = va.pca ,Y = y.tr[va.id,][is_early[va.id]])
lin.pred = predict (model, newdata=df.va.pca)
rmse = append(rmse,sqrt(mean((df.va.pca$Y-lin.pred)^2)) )
for(i in 1:99){
  add_to_model = as.numeric(substr(names(which.max(apply(df.trtr.pca[,-c(100,model_params)],2,function(x) abs(cor(x,model$residuals))))),5,10))
  model_params = as.vector(append(model_params,add_to_model))
  new_train = df.trtr.pca[,c(100, model_params)]
  model = lm(Y~.,data = new_train)
  
  lin.pred = predict (model, newdata=df.va.pca)
  rmse = append(rmse,sqrt(mean((df.va.pca$Y-lin.pred)^2)) )
}

plot(rmse)
min(rmse)



# do PCA and train linear regression on PCA -  adding columns by correlation - time_to_review
results_20 = c()
results_25 = c()
results_30 = c()
results_35 = c()
results_40 = c()
num_predictors = c()
for(validation in 1:100){
  va.id = sample (n,nva) # choose 2000 points for validation
  X.trtr = X.tr[-va.id,]
  X.trva = X.tr[va.id,]
  X.trtr_center <- apply(X.trtr,2,function(x) mean(x[x!=0]))
  X.tr_full<-X.trtr
  for(c in 1:ncol(X.tr)){
    X.tr_full[,c][X.tr_full[,c] == 0] <- X.trtr_center[c]
  }
  X.trva_center <- apply(X.trva,2,function(x) mean(x[x!=0]))
  X.trva_full<-X.trva
  for(c in 1:ncol(X.tr)){
    X.trva_full[,c][X.trva_full[,c] == 0] <- X.trva_center[c]
  }
  rmse = c()
  time_to_review = (y.da.tr[,1] - min(y.da.tr[,1]))/max(y.da.tr[,1])
#  time_to_review = (time_to_review-mean(time_to_review))/sd(time_to_review)
  train.pca = prcomp(X.tr_full, center = FALSE,scale. = TRUE)
  df.trtr.pca = data.frame(X = train.pca$x, TTR = time_to_review[-va.id], Y = y.tr[-va.id,])
  add_to_model = which.max(apply(df.trtr.pca[,-c(101,100)],2,function(x) abs(cor(x,df.trtr.pca['Y']))))
  model_params = as.vector(add_to_model)
  new_train = df.trtr.pca[,c(101,100, model_params)]
  model = lm(Y~TTR*(1+.),data = new_train)
  
  va.pca <- scale(X.trva_full, center= train.pca$center)
  va.pca <- va.pca %*% train.pca$rotation
  df.va.pca <- data.frame(X = va.pca ,TTR = time_to_review[va.id], Y = y.tr[va.id,])
  lin.pred = predict (model, newdata=df.va.pca)
  rmse = append(rmse,sqrt(mean((df.va.pca$Y-lin.pred)^2)) )
  for(i in 1:97){
    add_to_model = as.numeric(substr(names(which.max(apply(df.trtr.pca[,-c(101,100,model_params)],2,function(x) abs(cor(x,model$residuals))))),5,10))
    model_params = as.vector(append(model_params,add_to_model))
    new_train = df.trtr.pca[,c(101,100, model_params)]
    model = lm(Y~TTR*(1+.),data = new_train)
    
    lin.pred = predict (model, newdata=df.va.pca)
    rmse = append(rmse,sqrt(mean((y.tr[va.id,]-lin.pred)^2)) )
  }
  
  plot(rmse)
  print(min(rmse))
  num_predictors = append(num_predictors,which.min(rmse))
  results_20 = append(results_20,rmse[20])
  results_25 = append(results_25,rmse[25])
  results_30 = append(results_30,rmse[30])
  results_35 = append(results_35,rmse[35])
  results_40 = append(results_40,rmse[40])
}


# do PCA and KNN
rmse = c()
train.pca = prcomp(X.tr_full[-va.id,], center = TRUE,scale. = TRUE)
df.trtr.pca = data.frame(X = train.pca$x, TTR = time_to_review[-va.id], Y = y.tr[-va.id,])
cos.sim <- function(A,B) 
{
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}  
va.pca <- scale(X.tr_full[va.id,], center= train.pca$center)
va.pca <- va.pca %*% train.pca$rotation
nval<- nrow(va.pca)



rmse = c()
for(k in c(10)){
  predictions<-rep(NA,nval)
  for(i in 1:nval){
    similarities <- apply(df.trtr.pca[,1:10],1, function(x) cos.sim(x,va.pca[i,1:10]))
    predictions[i]<-mean(df.trtr.pca[which(similarities %in% sort(similarities, decreasing=F)[1:k]),"Y"])
  }
  rmse<- append(rmse, sqrt(mean((va$y-predictions)^2)))
  print(k)
}
plot(rmse)

