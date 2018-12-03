obs.numb.dig <-
function(dat,dig=1){

	prep<-function(dat){dat=as.data.frame(dat); rownb=dim(dat)[1]; colnb=dim(dat)[2];
	for (j in 1:colnb) if (is.numeric(dat[,j])==FALSE) dat[,j]=as.numeric(as.character(dat[,j])); 
	for (i in 1:rownb) for (j in 1:colnb) if (is.na(dat[i,j])==TRUE) dat[i,j]=0;
	return(dat);}

	firstdigit<-function(n){n=abs(n); if (n>=1) {k=1; while(n>=10**k) k=k+1; return(floor(n/10**(k-1)))} 
					else {k=1; while(n*10**k<1) k=k+1; return(floor(n*10**k))}}

	secondigit<-function(n){n=abs(n); if (n>=10) {k=1; while(n>=10**k) k=k+1; return(floor(n/10**(k-2))-10*floor(n/10**(k-1)))} 
					else if(n>=1) {if (floor(n)==n) return("?") else return(floor(10*n)-10*floor(n))} 
					else {k=1; while(n*10**k<1) k=k+1; if(floor(n*10**k)==n*10**k) return("?") else return(floor(n*10**(k+1))-10*floor(n*10**k))}}

	thirdigit<-function(n){n=abs(n); if (n>=100) {k=1;while (n>=10**k) k=k+1; return(floor(n/10**(k-3))-10*floor(n/10**(k-2)))} 
					else if(n>=10) {if (floor(n)==n) return("?") else return(floor(10*n)-10*floor(n))} 
					else if(n>=1) {if (floor(n*10)==n*10) return("?") else return(floor(100*n)-10*floor(10*n))} 
					else {k=1; while(n*10**k<1) k=k+1; if(floor(n*10**(k+1))==n*10**(k+1)) return("?") else return(floor(n*10**(k+2))-10*floor(n*10**(k+1)))}}

	fourthdigit<-function(n){n=abs(n); if (n>=1000) {k=1;while (n>=10**k) k=k+1; return(floor(n/10**(k-4))-10*floor(n/10**(k-3)))} 
					else if(n>=100) {if (floor(n)==n) return("?") else return(floor(10*n)-10*floor(n))} 
					else if(n>=10) {if (floor(n*10)==n*10) return("?") else return(floor(100*n)-10*floor(10*n))} 
					else if(n>=1) {if (floor(n*100)==n*100) return("?") else return(floor(1000*n)-10*floor(n*100))} 
					else {k=1; while(n*10**k<1) k=k+1; if(floor(n*10**(k+2))==n*10**(k+2)) return("?") else return(floor(n*10**(k+3))-10*floor(n*10**(k+2)))}}

	digit<-function(n,dig=1){if(n==0) return("?") 
				else if(dig==1) return(firstdigit(n)) else if(dig==2) return(secondigit(n)) else if(dig==3) return(thirdigit(n)) else return(fourthdigit(n))}

dat=prep(dat); 
if (dig==1) {vecnum=rep(0,9); for (i in 1:dim(dat)[1]) {for (j in 1:dim(dat)[2]) if (digit(dat[i,j])!="?") vecnum[digit(dat[i,j])]=vecnum[digit(dat[i,j])]+1}; return(vecnum)} 
else {vecnum=rep(0,10); for (i in 1:dim(dat)[1]) {for (j in 1:dim(dat)[2]) if (digit(dat[i,j],dig=dig)!="?") vecnum[digit(dat[i,j],dig=dig)+1]=vecnum[digit(dat[i,j],dig=dig)+1]+1}; 
return(vecnum)}}
