remove(list=ls())
zpath<-("//alroseserver/HomeFolders/JacoboP/JacoboP$/customers")
setwd(zpath)
#source("mainprog.R")

#####################
#READ AND MODIFY DATA
#####################

	test<-read.csv("inv_data.csv",sep=",")

	test.mod<-test[,c(1,2,5,7,8)]
	colnames(test.mod)<-c("id_number","date","origin","auntaxed", "atotal")
	v<-0
	for (i in 1:length(test.mod[,1])){
		v[i]<-length(subset(test.mod, factor(test.mod[,1])==test.mod[i,1])[,1])
	}
	test.mod.new<-cbind(test.mod,v)
	colnames(test.mod.new)<-c("id_number","date","origin","auntaxed", "atotal","freq")
	test.mod.new<-test.mod.new[c(1,6,2:5)]
	test.mod.date<-test.mod.new[order(test.mod.new$id_number,test.mod.new$date),]
	test.mod.date$date<-as.Date(test.mod.date$date)
	
	w<-0
	for (i in 1:length(test.mod.date[,1])){
		test.mod[i,]
		w[i]<-max(subset(test.mod.date, factor(test.mod.date[,1])==test.mod.date[i,1])[,3])-test.mod.date[i,3]
	}
	test.mod.new.date<-cbind(test.mod.date,w)
	colnames(test.mod.new.date)<-c("id_number","freq","date","origin","auntaxed", "atotal","difftime")
	testa<-test.mod.new.date[,c(1,2,7,3:6)]

#############################
## 6 freq

mylist<-list()
num<-max(testa$date)
for (i in 1:16){
	freq.in<-i
	dff<-as.data.frame(subset(testa, testa$freq==freq.in,select=difftime))
	
	v<-matrix(rep(0,nrow(dff)/freq.in),nrow=1)
	
	for (j in 1:freq.in){
		c<-freq.in+1-j
		dff<-as.data.frame(subset(testa, testa$freq==freq.in,select=difftime))
		v<-rbind(v,as.integer(dff[seq(c,nrow(dff),freq.in),1]))
	}
w<-nrow(v)
B<-v[2:w,]
mylist[[i]]<-B	
}


#############################
	tabl_idnumber <- table(test.mod$id_number)
	
	df11<-as.data.frame(tabl_idnumber)
	colnames(df11)<-c("cust_id","number_orders")

	number_orders<-length(test.mod$id_number)


	tabl_idnumber_amounth<-table(test.mod$id_number,test.mod$auntaxed)
	objs<-list(test.mod,df11,number_orders,tabl_idnumber)

##########################
#GO ON WITH ADDING R-FILES
##########################
source("targetgroupsorders.R")
source("cust_id.R")


#print(objects())
