con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_ratings_all.dat")
X.tr = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2018/train_y_rating.dat")
y.tr = read.table (con)

train_full <- cbind(X.tr, y.tr)


#### create sets for  i2v
which(train_full == 2)
dim(train_full)
list_of_sets = list() 
for (i in 0:5) {
  list_of_sets = append(list_of_sets,apply(train_full, 1,  function(x) names[which(x == 5)]))
}



## export to txt, load in python and run w2v model