
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
va.pca <- scale(X.tr_full[va.id,], center= train.pca$center)
va.pca <- va.pca %*% train.pca$rotation
for(k in 1:50){
  df.trtr.pca = data.frame(X = train.pca$x[,1:k],Y = y.tr[-va.id,])
  pca_lin_mod = lm(Y~., data = df.trtr.pca)
  
  df.va.pca = data.frame(X = va.pca[,1:k],Y = y.tr[va.id,])
  
  lin.pred = predict (pca_lin_mod, newdata=df.va.pca)
  rmse = append(rmse,sqrt(mean((df.va.pca$Y-lin.pred)^2)))
}

df.trtr.pca = data.frame(X = train.pca$x[,1:k],Y = y.tr[-va.id,])
add_to_model = which.max(apply(df.trtr.pca[,-100],2,function(x) abs(cor(x,trtr['Y']))))
model_params = as.vector(add_to_model)
new_train = trtr[,c(100, model_params)]
model = lm(y~.,data = new_train)
lin.pred = predict (model, newdata=va)
rmse = append(rmse,sqrt(mean((va$y-lin.pred)^2)) )
ignore_params = c()
for(i in 1:99){
  add_to_model = as.numeric(substr(names(which.max(apply(trtr[,-c(100,model_params)],2,function(x) abs(cor(x,model$residuals))))),4,10))
  model_params = as.vector(append(model_params,add_to_model))
  new_train = trtr[,c(100, model_params)]
  model = lm(y~.,data = new_train)
  
  lin.pred = predict (model, newdata=va)
  rmse = append(rmse,sqrt(mean((va$y-lin.pred)^2)) )
}

plot(rmse)
plot(rmse)
min(rmse)