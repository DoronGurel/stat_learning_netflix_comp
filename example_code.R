
########### Training data (rankings only, no dates):
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_ratings_all.dat")
X.tr = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_y_rating.dat")
y.tr = read.table (con)

con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_y_date.dat")
y.da.tr = read.table (con)


con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/movie_titles.txt")
titles = read.table(con,sep=",")
names (X.tr) = substr(as.character(titles[,2]),1,10)


########### Get to know our data a little:
table (y.tr) # What rankings does our target get?
apply(data.frame(X.tr[,1:14],y.tr),2,mean) # Which movies are liked?
cor(y.tr,X.tr[,1:14]) # which movies correlated with Miss Congeniality?
apply (X.tr==0, 2, sum) # how many missing?
cor (y.tr, y.da.tr) # changes with time?



########### Divide training data into training and validation
n = dim(X.tr)[1]
nva = 2000
ntr = n-nva
va.id = sample (n,nva) # choose 2000 points for validation
#trtr = data.frame (X = X.tr[-va.id,], yda=y.da.tr[-va.id,], y=y.tr[-va.id,]) # include dates
trtr = data.frame (X = X.tr[-va.id,],y=y.tr[-va.id,])
#va = data.frame (X = X.tr[va.id,], yda=y.da.tr[va.id,], y=y.tr[va.id,]) #include dates
va = data.frame (X = X.tr[va.id,],y=y.tr[va.id,])


########### baseline RMSE
sqrt(mean((va$y-mean(trtr$y))^2))

########### regression on all rankings (with missing = 0!!!!)
lin.mod = lm (y~.,data=trtr)

########### in-sample RMSE
lin.insamp = predict (lin.mod)
sqrt(mean((trtr$y-lin.insamp)^2))

########### RMSE on validation data
lin.pred = predict (lin.mod, newdata=va)
sqrt(mean((va$y-lin.pred)^2))

########### rankings can't be higher than 5!
lin.pred.cap = pmin (lin.pred,5)
sqrt(mean((va$y-lin.pred.cap)^2))



########### regression on complete data only (14 movies)
lin.mod1 = lm (y~.,data=trtr[,c(1:14,100)])

########### RMSE on validation data
lin.pred1 = predict (lin.mod1, newdata=va)
sqrt(mean((va$y-lin.pred1)^2)) 



########### Output of linear models:
summary(lin.mod)
summary(lin.mod1)

