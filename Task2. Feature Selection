reuters111 <- read.csv("F:/R_WorkingSpace/courseWork/FinalProcessed.csv")
library("Matrix")
library(topicmodels)
library(tm)
library("FSelector")

#delete instances which do not have texts.
row.del<-c(which(reuters111[,124] == ""),which(is.na(reuters111[,124])))
reuters111<-reuters111[-row.del,]

#Get the corpus and delete features according to the sparsity
the_vector<-VectorSource(reuters111[,124])
the_corpus<-Corpus(the_vector)
dtm<-DocumentTermMatrix(the_corpus)
dtm2<-removeSparseTerms(dtm,sparse=0.95)
df_dtm2<-as.data.frame(inspect(dtm2))

write.csv(df_dtm2, "DTM2.csv", row.names = F)


#Create the LDA model, and use 10 topics to represent the whole corpus
lda<-LDA(dtm, control = list(alpha = 0.1), k = 10, method = "VEM")
term<-terms(lda,20)
#Get the top 20 terms for each topic and delete those repeated terms
term_lda<-c(term[,1],term[,2],term[,3],term[,4],term[,5],term[,6],term[,7],term[,8],term[,9],term[,10])
get_uniq<-unique(term_lda)
#Get the topic list of the LDA model
topic.lda<-topics(lda)
#Manually create the LDA Document Term Matrix
#I assume that the document which belongs to a specific topic would have all those terms in that topic
lda.feature<-as.data.frame(matrix(NA,length(topic.lda),length(get_uniq)))
colnames(lda.feature)<-get_uniq

for(h in 1:length(topic.lda)){
  matchPoint<-match(term[,topic.lda[h]], get_uniq)
  lda.feature[h,]<-0
  lda.feature[h,matchPoint]<-1
  print(h)
}
#Delete those terms which appears in every documents, because this kind of term is meaningless.
NumColLDA<-ncol(lda.feature)
avector<-c(0)
for(i in 1:NumColLDA){
  avector<-c(avector,sum(lda.feature[,i]))
}
avector<-avector[-1]
lda.feature<-lda.feature[,-which(avector == 10369)]

#通过LDA得到新的feature
#每次在同一个语料库调用LDA会得到不同的结果
write.csv(lda.feature, "LDA_Feature.csv", row.names = F)
write.csv(lda.feature, "LDA_Feature_JUSTFORTEST.csv", row.names = F)

LDA_Feature <- read.csv("F:/R_WorkingSpace/courseWork/LDA_Feature.csv")

#delete the zero row.得到dtm3，实际上经过前面对于最后一列的去空处理之后，dtm2和dtm3是一样的
#写这一步是因为最开始的时候没有处理那些处理的text为空的行，现在在前面处理了，所以在这个地方保持了一致
NumRow<-nrow(df_dtm2)
NumCol<-ncol(df_dtm2)

#vector<-c(0)  
#for(i in 1:NumRow){
#  x<-sum(df_dtm2[i,1:NumCol])
#  if(x == 0)vector<-append(vector,i,after = length(vector))
#  print(i)
#}
#vector<-vector[-1]
#df_dtm3<-df_dtm2[-vector,]
#reuters222<-reuters111[-vector,]
#write.csv(reuters222, "reutersDeleteRows.csv", row.names = F)
#对自己取出的feature计算TF-IDF然后去除一部分数据，不对LDA数据做该处理是因为认为lda数据已经比较精简了

#Compute TF
tf.dtm3<-df_dtm2
for(a in 1:NumCol){
  tf.dtm3[,a]<-tf.dtm3[,a]/max(tf.dtm3[,a])
}
#Compute IDF
num.doc<-c(1:NumCol)
idf<-c(1:NumCol)
for(b in 1:NumCol){
 num.doc[b]<-length(which(df_dtm2[,b] != 0))
 idf[b]<-log(NumRow/num.doc[b])
}
#Compute TF*IDF
tfidf<-df_dtm2
for(c in 1:NumCol){
  tfidf[,c]<-tf.dtm3[,c]*idf[c]
}
#根据TF*IDF取出每列
skt<-c(1:NumRow)
for(edg in 1:NumRow){
  skt[edg]<-length(which(tfidf[edg,] != 0))
}

summary(skt)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
2.00   12.00   18.00   21.34   26.00   87.00

tfidf.copy<-tfidf
write.csv(tfidf.copy, "TFIDF_COPY.csv", row.names = F)

#选取前18个属性，因为中位数为18，前18个属性如果都有值的话全部换成1，如果没有值得话则保持0
tfidf<-tfidf.copy
for(h in 1:NumRow){
  len<-length(which(tfidf[h,] != 0))
  candidate<-order(tfidf[h,],decreasing = TRUE)
  #candidate1<-order(tfidf[h,],decreasing = TRUE)[1:18]
  #candidate2<-order(tfidf[h,],decreasing = TRUE)[19:180]
  if(len >= 18){
    tfidf[h,candidate][1:18]<-1
    tfidf[h,candidate][19:NumCol]<-0
  } else {
    tfidf[h,candidate][1:len]<-1
    tfidf[h,candidate][(len+1):NumCol]<-0
  }
}
write.csv(tfidf, "TfIdf.csv", row.names = F)

#上面的函数运行完之后tfidf被更新成0和1的格式
#删除加和为0的列
for(i in 1:NumCol){
  x<-sum(tfidf[,i])
  #if(x <= 500)print(i)
  print(x)
}
#没有0
#为了选择feature，决定选择加和大于等于中位数的列
alist<-c(0)
for(i in 1:NumCol){
  x<-sum(tfidf[,i])
  alist<-append(alist,x,after = length(alist))
}
alist<-alist[-1]
delcolumn<-which(alist<median(alist))
tfidf.del<-tfidf[,-delcolumn]
#对剩余属性排序后计算chi-square
blist<-c(0)
for(i in 1:90){
  x<-sum(tfidf.del[,i])
  blist<-append(blist,x,after = length(blist))
}
blist<-blist[-1]
the.order<-order(blist,decreasing = T)

chi.matrix<-as.data.frame(matrix(0,90,90))
for(i in 1:90){
  for(k in 1:90){
    if(i == k)chi.matrix[i,k] <- 0
    else{
      chi.matrix[i,k]<-chi.squared(as.factor(tfidf.del[,i])~as.factor(tfidf.del[,k]),tfidf.del)
    }
  }
}
write.csv(chi.matrix, "ChiMatrix.csv", row.names = F)


for(ig in 1:90){
  print(max(chi.matrix[,ig]))
}
#所有的值都小于1，独立，不需要额外删除

write.csv(tfidf.del, "featureSelected.csv", row.names = F)


#Get the combined data set
lda.feature <- read.csv("F:/R_WorkingSpace/courseWork/LDA_Feature.csv")

colNameList<-c(colnames(lda.feature),colnames(featureSelected))
colNameUniq<-unique(colNameList)

data.combined<-as.data.frame(matrix(0,10369,length(colNameUniq)))
colnames(data.combined)<-colNameUniq
CombineColName<-colnames(data.combined)

for( h in 1:10369){
  match.spot.feature <- match(CombineColName, colnames(featureSelected)[which(featureSelected[h,] == 1)], nomatch = 0) != 0
  match.spot.lda<- match(CombineColName, colnames(lda.feature)[which(lda.feature[h,] == 1)], nomatch = 0) != 0 
  data.combined[h,match.spot.feature]<-1
  data.combined[h,match.spot.lda]<-1
  print(h)
}

write.csv(data.combined, "DataCombine.csv", row.names = F)


