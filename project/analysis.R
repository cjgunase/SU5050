data<-read.csv("./file_phoe.csv")
summary(data[,8])
data$street<-as.factor(data$street)
data$lot<-as.factor(data$lot)



# ------------------------------
# Load the data however you want
# ------------------------------

# I called my data dsBase, here I copy the data to dsBase.iqr
# I wanted to keep a copy of the original data set
dsBase.iqr <- data

# Create a variable/vector/collection of the column names you want to remove outliers on.
vars <- c("score")

# Create a variable to store the row id's to be removed
Outliers <- c()

# Loop through the list of columns you specified
for(i in vars){
  
  # Get the Min/Max values
  max <- quantile(dsBase.iqr[,i],0.75, na.rm=TRUE) + (IQR(dsBase.iqr[,i], na.rm=TRUE) * 1.5 )
  min <- quantile(dsBase.iqr[,i],0.25, na.rm=TRUE) - (IQR(dsBase.iqr[,i], na.rm=TRUE) * 1.5 )
  
  # Get the id's using which
  idx <- which(dsBase.iqr[,i] < min | dsBase.iqr[,i] > max)
  
  # Output the number of outliers in each variable
  print(paste(i, length(idx), sep=''))
  
  # Append the outliers list
  Outliers <- c(Outliers, idx) 
}

# Sort, I think it's always good to do this
Outliers <- sort(Outliers)

# Remove the outliers
dsBase.iqr <- dsBase.iqr[-Outliers,]

library(caret)
preObj <- preProcess(dsBase.iqr[, 3], method=c("center", "scale"))
newData <- predict(preObj, data[, -10])




## 75% of the sample size
smp_size <- floor(0.75 * nrow(dsBase.iqr))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dsBase.iqr)), size = smp_size)

train <- dsBase.iqr[train_ind, ]
test <- dsBase.iqr[-train_ind, ]



names(data)
library(tree)
tree.model <- tree(score ~ street+lot+dep_stores+rest+shopping+hotel+art, data=train)
plot(tree.model)
text(tree.model, cex=.75)
pred<-predict(tree.model,test)

sqrt( sum( (pred - test$score)^2 , na.rm = TRUE ) / nrow(test) )






(sqrt( sum( (pred - test$score)^2 , na.rm = TRUE ) / nrow(test) ) - min(dsBase.iqr[,8]))/(max(dsBase.iqr[,8])-min(dsBase.iqr[,8]))
#Not Good
lm.model <- lm(score ~ street+lot+dep_stores+rest+shopping+hotel+art,data=train)
pred<-predict(lm.model,test)
sqrt( sum( (pred - test$score)^2 , na.rm = TRUE ) / nrow(test) )




res_loc<-read.table("./restaurent_locations_phoenix.csv", sep=",", header=T)
res_loc<-read.table("./res_loc_Phenix.csv", sep=",", header=T)
res_loc.dists <- as.matrix(dist(cbind(res_loc$lon, res_loc$lat)))
#install.packages("ape")
library(ape)
res_loc.dists.bin <- (res_loc.dists > 0.1 & res_loc.dists <= 0.5)
Moran.I(res_loc$dec_score_num, res_loc.dists.bin)

#install.packages("geoR")
library(geoR)

head(res_loc, n=10)
dists <- dist(res_loc[,3:4])
summary(dists)
breaks = seq(0, 1.5, l = 11)
v1 <- variog(coords = res_loc[,3:4], data = res_loc[,9], breaks = breaks)
?dists
v1.summary <- cbind(c(1:6), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi-variance", "# of pairs")
v1.summary
plot(v1, type = "b", main = "Variogram: Restaurent score in Pheonix AZ")
summary(res_loc[,5])

install.packages("gtools")
library(gtools)
quartiles <- quantcut( res_loc[,4])
table(quartiles)
install.packages("plyr")
library(plyr)

res_loc$dec_score<-revalue(quartiles, c("[3,24]"="green","(24,70]"="blue","(70,228]"="yellow","(228,5.81e+03]"="red"))
res_loc$dec_score_num<-as.numeric(revalue(quartiles, c("[3,24]"=1,"(24,70]"=2,"(70,228]"=3,"(228,5.81e+03]"=4)))
write.csv(res_loc,file = "res_loc_Phenix.csv")

as.factor(quartiles)
table(quartiles)

