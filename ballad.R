library(RSQLite)
library(RMySQL)
library(stylo)
library(tm)
mydb = dbConnect(MySQL(), user='root', password='******', dbname='ballad', host='localhost')

setwd("~/ballad")

dbListTables(mydb)
author = dbGetQuery(mydb, "SELECT * FROM ballad_authors")
names(author)
balladID  = dbGetQuery(mydb, "SELECT BDA_BDID FROM ballad_authors")
fulltext = dbGetQuery(mydb, "SELECT * FROM ballad_fulltext")
bidf = dbGetQuery(mydb, "SELECT BDFT_BDID FROM ballad_fulltext")
textf = dbGetQuery(mydb, "SELECT BDFT_Text FROM ballad_fulltext")


combine = dbGetQuery(mydb, "SELECT a.BDA_BDID, a.BDA_AUID, b.BDFT_Text FROM ballad_fulltext AS b, ballad_authors AS a 
           WHERE b.BDFT_BDID = a.BDA_BDID")

sapply(1:dim(combine)[1],function(i) write.table(combine$BDFT_Text[i],row.names = FALSE, col.names = FALSE, quote = FALSE,
                                                 file = paste0(paste(i, combine$BDA_AUID[i], combine$BDA_BDID[i], sep = "_"),".txt")))



setwd("~/ballad_test")

testtext = fulltext[!fulltext$BDFT_BDID%in%combine$BDA_BDID,]

sapply(1:dim(testtext)[1],function(i) write.table(testtext$BDFT_Text[i],row.names = FALSE, col.names = FALSE, quote = FALSE,
                                                 file = paste0(paste(i, testtext$BDFT_BDID[i], sep = "_"),".txt")))



setTRAIN = strsplit(combine$BDFT_Text, split = "[[:space:]]|[[:punct:]]")
setTRAIN = lapply(1:length(setTRIAN), function(j) tolower(lapply(1:length(setTRIAN), function(i) setTRIAN[[i]][setTRIAN[[i]]!=""])[[j]]))

authorid = as.vector(combine$BDA_AUID)
setTRAIN2 = setTRAIN
names(setTRAIN2) =authorid
names(setTRAIN) = paste(authorid, combine$BDA_BDID, sep = "_")
length(setTRAIN)

getname = c("4","8","10")
getloc = lapply(1:length(getname), function(i) which(names(setTRAIN2) == getname[i]))
newtrain = c(setTRAIN[getloc[[1]][1:4]],setTRAIN[getloc[[2]][1:4]],setTRAIN[getloc[[3]][1:4]])
names(newtrain)
#more step:delete them in the train 
for (i in 1:length(newtrain))
{
  setTRAIN = setTRAIN[names(setTRAIN) != names(newtrain)[i]]
}


setTEST = strsplit(testtext$BDFT_Text, split = "[[:space:]]|[[:punct:]]")
setTEST = lapply(1:length(setTEST), function(i) setTEST[[i]][setTEST[[i]]!=""])
setTEST= lapply(1:length(setTEST), function(j) tolower(setTEST[[j]]))
length(setTEST)
#give each test file name: anonymous_BDID
names(setTEST) = paste("anonymous",testtext$BDFT_BDID, sep = "_")
test = c(setTEST,newtrain)
train = setTRAIN
#change the train name as to only authorid
split = strsplit(names(train), "_")
names(train) = unlist(lapply(1:length(split), function(i)split[[i]][1]))

classify(training.corpus = train, test.corpus = test)






setwd("~/test")
#procedure for testing
repeatedID = table(names(setTRAIN2))[table(names(setTRAIN2))>1]
repeatedname = names(repeatedID)
getloct = lapply(1:length(repeatedname), function(i) which(names(setTRAIN2) == repeatedname[i]))
newtraint = sapply(1:length(getloct), function(i) setTRAIN[getloct[[i]][1:length(getloct[[i]])-1]])
traint = character(0)
for (i in 1:length(newtraint)){
  traint = c(traint,newtraint[[i]])
}
singleID = table(names(setTRAIN2))[table(names(setTRAIN2))==1]
singlename = names(singleID)
getsingle = setTRAIN[singlename]
traint = c(getsingle,traint)
testt = sapply(1:length(getloct), function(i) setTRAIN2[getloct[[i]][length(getloct[[i]])]])
names(testt) =  paste0("annonymous", names(testt))
classify(training.corpus = train, test.corpus = test)

final_results <- read.delim("C:/Users/bian0553/Desktop/SQLproject/test/final_results.txt", header=FALSE, comment.char="#")
final = final_results[-(107:111),]
final[,1]=gsub("[a-z]","\\1",final[,1])
final[,1]=as.numeric(final[,1])
table(final[,1] == final[,3])[2]/length(final[,1])

#precision rate: delta &knn: 0.4245283, svm:0.4056604, naivebayes:0.08, delta(cosine):0.4528302 



#Use svm
library(RTextTools)
doc_matrix = create_matrix(train)
container <- create_container(doc_matrix, names(train), trainSize=1:965, virgin=FALSE) 
model <- train_model(container, "SVM", kernel="linear", cost=1)
# create a prediction document term matrix 
predMatrix <- create_matrix(test, originalMatrix=doc_matrix) 


# create the corresponding container
predSize = length(test);
predictionContainer <- create_container(predMatrix, labels=names(test), testSize=1:predSize, virgin=FALSE) 


results <- classify_model(predictionContainer, model)
class(results)
View(results)
write.csv(results ,file = "classification.csv")





#do test
repeatedID = table(names(setTRAIN2))[table(names(setTRAIN2))>1]
repeatedname = names(repeatedID)
getloct = lapply(1:length(repeatedname), function(i) which(names(setTRAIN2) == repeatedname[i]))
newtraint = sapply(1:length(getloct), function(i) setTRAIN2[getloct[[i]][1:length(getloct[[i]])-1]])
traint = character(0)
for (i in 1:length(newtraint)){
  traint = c(traint,newtraint[[i]])
}
singleID = table(names(setTRAIN2))[table(names(setTRAIN2))==1]
singlename = names(singleID)
getsingle = setTRAIN2[singlename]
traint = c(getsingle,traint)
testt = sapply(1:length(getloct), function(i) setTRAIN2[getloct[[i]][length(getloct[[i]])]])
names(testt) =  paste0("annonymous", names(testt))

dtmatrix = create_matrix(traint)
containert <- create_container(dtmatrix, names(traint), trainSize=1:length(traint), virgin=FALSE) 
modelt <- train_model(containert, "SVM", kernel="linear", cost=1)
# create a prediction document term matrix 
predMatrixt <- create_matrix(testt, originalMatrix=dtmatrix) 
# create the corresponding container
predSizet = length(testt);
predictionContainert <- create_container(predMatrixt, labels=names(testt), testSize=1:predSizet, virgin=FALSE) 
resultst <- classify_model(predictionContainert, modelt)
resultst$test = names(testt)
View(resultst)
resultst[,3]=gsub("[a-z]","\\1",resultst[,3])
resultst[,3]=as.numeric(resultst[,3])
table(resultst[,3] == resultst[,1])[2]/length(resultst[,1])




#ESTC
ballads = dbGetQuery(mydb, "SELECT BD_ID, BD_ESTCID FROM ballads")
names(ballads)
dim(ballads)
length(unique(ballads$BD_ESTCID))
#subset to get with ESTCID and without ESTCID
without0 = ballads[is.na(ballads$BD_ESTCID),]
with = ballads[!is.na(ballads$BD_ESTCID),]
#get the list: one ESTCID to many ballads
group = split(with$BD_ID,with$BD_ESTCID)



bidf = dbGetQuery(mydb, "SELECT BDFT_BDID FROM ballad_fulltext")
dim(bidf)
table(with$BD_ID%in%bidf$BDFT_BDID)
table(without$BD_ID%in%bidf$BDFT_BDID)

textf = dbGetQuery(mydb, "SELECT * FROM ballad_fulltext")
a = textf[1,1]
b = textf[2,1]
corpus <- Corpus(VectorSource(c(a,b)))
corpus  # check corpus

library(SnowballC)
td.mat <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = TRUE,tolower = TRUE))
inspect(td.mat)
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat  # check distance matrix

library(proxy)

distance = function(text1,text2){
  corpus <- Corpus(VectorSource(c(text1,text2)))
  td.mat <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, 
                                                      stopwords = TRUE,tolower = TRUE))
  dist.mat <- dist(t(as.matrix(td.mat)))  #,method = algorithm)
  dist.mat  # check distance matrix
}



#loop through the text without ESTC_ID and check if there is a match with other text with ESTC_ID
textin = 0
textout = 0
for(i in 1:dim(without)[1]){
  if(without$BD_ID[i]%in%bidf$BDFT_BDID){
    textout = textf[textf$BDFT_BDID == without$BD_ID[i],2]
    if(length(textout) > 1){textout = textout[1]}
  }else{next}
      
  for(j in 1:length(group)){
    for(k in length(group[[j]])){
      if(group[[j]][k]%in%bidf$BDFT_BDID){
        textin = textf[textf$BDFT_BDID == group[[j]][k],2]
        if(length(textin) > 1){textin = textin[1]}  
        if(distance(textout, textin) < 20){
            without$new_ESTCID[without$BD_ID == without$BD_ID[i]] = names(group[j]) 
            print(j)
        }  
      }
      print(paste0("j is", j))
    }
    }
print(paste0("i is ",i))  
}



write.table(without[,-2],file = "ESTCID predict")



#Then match the ballads without the ESTC ID
#the ballad ID without the ESTCID
withoutid = without[is.na(without$new_ESTCID),1]   #238
textout = character(0)
textin = character(0)
withoutid = withoutid[withoutid%in%bidf$BDFT_BDID]  #154


num = 0
combine = list()
for(i in 1:length(withoutid)){
    textout = textf[textf$BDFT_BDID == withoutid[i],2]
    if(length(textout) > 1){textout = textout[1]}
    newwithoutid = withoutid[-i]
    combine = append(combine,withoutid[i])
  for(j in 1:length(newwithoutid)){
   textin = textf[textf$BDFT_BDID == newwithoutid[j],2]
    #print(distance(textout,textin))
    if(distance(textout, textin) < 20){
      num = num+1
      combine[[i]] = append(combine[[i]],newwithoutid[j])
    }
  }
}


table(lapply(1:145,function(i)length(combine[[i]])) > 1)  #20 successfully matched
without2 = without
without2
num = 0
for(i in 1:length(combine)){
  if(length(combine[[i]]) > 1){
    print(i)
    num = num + 1
    print("----")
    print(num)
    for(j in 1:length(combine[[i]])){
      without2[without2$BD_ID == combine[[i]][j],3] = num 
    }
}
}



table(is.na(without2$new_ESTCID))
without2  = without2[,-2]
write.table(without2,file = "ESTCID predict2")

#who doesn't have text
withouttext = without$BD_ID[!(without$BD_ID%in%bidf$BDFT_BDID)]


#total unmatched
without3 = without2[is.na(without2$new_ESTCID),]
#excluding the unmatched because of missing text
unmatched = without3$BD_ID[!without3$BD_ID%in%withouttext]  #145 unmatched

###create table and insert query
man = cbind(1:4738,unique(ballads$BD_ESTCID[!is.na(ballads$BD_ESTCID)]))
man = as.data.frame(man)
names(man) = c("MN_ID","MN_ESTCID")
man$MN_ID = as.numeric(man$MN_ID)
head(dbGetQuery(mydb, "SELECT * FROM manifestations"))
dbWriteTable(mydb, "manifestations",man,
             row.names = FALSE, overwrite = TRUE)

match = without2[!is.na(without2$new_ESTCID),]

col1 = 1:dim(without0[!without0$BD_ID%in%withouttext,])[1]
col2 = without0[!without0$BD_ID%in%withouttext,]$BD_ID
col3 = vector()
col4 = vector()
for(i in 1:length(col2)){
  if(col2[i]%in%unmatched){
    col3[i] = "FALSE"
    col4[i] = NA
  }else{
    col3[i] = "TRUE"
    estc = match$new_ESTCID[match$BD_ID==col2[i]]
    col4[i] = man$MN_ID[man$MN_ESTCID == estc]
  }  
}
col3 = as.logical(col3)

mi = as.data.frame(cbind(col1,col2,col3,col4))
mi$col3 = as.logical(mi$col3)
names(mi) = c("MI_ID","MI_BDID","MI_DIST_MATCH","MI_MNID")
dbWriteTable(mydb, "manifestations_items",mi,
             row.names = FALSE, overwrite = TRUE)
head(dbGetQuery(mydb, "SELECT * FROM manifestations_items"))

write.table(withouttext,file = "missing text")
