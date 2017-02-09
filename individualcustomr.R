#####################
#INDIVIDUAL CUSTORMER
#####################

customer_idnumber<- function(cust_id_keyboard){
	
	count<-0
	count_mean<-0
	v<-c(rep(0,ncol(test.mod)))
	cust_id_ord<-c(0)
	matrix_cust<-matrix(c(rep(0,ncol(test.mod))), ncol=ncol(test.mod))


	for (j in 1:length(test.mod$id_number)){
		if ((test.mod$id_number[j]==cust_id_keyboard)== TRUE){
			count<-count+1
			v<-matrix((test.mod[j,]),ncol=ncol(test.mod))
			v[2]<-as.Date(test.mod$date[j])
			v[4]<-as.double(test.mod$auntaxed[j])
			matrix_cust<-rbind(matrix_cust,v)
			if(v[4]!=0) count_mean<-count_mean+1
		}
	}
	if(count==0){
		print("Customer do not exist")
	}else{
		cum_sum<-sum(as.double(matrix_cust[,4]))	
		newmean<-mean(as.double(matrix_cust[2:nrow(matrix_cust),4],na.rm=TRUE))
		newstandardev<-sd(as.double(matrix_cust[2:nrow(matrix_cust),4],na.rm=TRUE))

#Calculate the number of orders per this customer (count)
#mean (cum_sum/count/mean)
#and other mean with 0's means and sdevs

		return(c(count,cum_sum/count_mean,newmean,newstandardev))
	}
