##########################
#GROUPS (BY NUMBER ORDERS)
##########################

group_num_ord<-function(x){
	elements<-0
	elementsplus<-0
	elementsminus<-0
	vector<-0
	vplus<-0
	vminus<-0

	for (i in 1:max(df11$number_orders)){
		
		elements[i]<-list(subset(df11,df11$number_orders==i, select=cust_id))
		elementsplus[i]<-list(subset(df11,df11$number_orders>i, select=cust_id))
		elementsminus[i]<-list(subset(df11,df11$number_orders<i, select=cust_id))
	
	}

	vect<-elements[x]
	vectp<-elementsplus[x]
	vectm<-elementsminus[x]
	vector<-as.double(as.matrix(vect[[1]]))
	vplus<-as.double(as.matrix(vectp[[1]]))
	vminus<-as.double(as.matrix(vectm[[1]]))

	mean1<-mean(subset(test.mod, test.mod[,1]%in%vector)[,4])
	mean2<-mean(subset(test.mod, test.mod[,1]%in%vplus)[,4])
	mean3<-mean(subset(test.mod, test.mod[,1]%in%vminus)[,4])

	cust<-list(vector, vplus, vminus)
	cust2<-list(length(vector),length(vplus),length(vminus))
	
	return(c(mean1,mean2,mean3))
}
