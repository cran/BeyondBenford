theor.distr.val <-
function(upbound,dig=1){if (upbound>=10**(dig-1)) {val=rep(0,upbound-10**(dig-1)+1); 
for (k in 10**(dig-1):upbound) {sum=0; for (i in (k+1-10**(dig-1)):(upbound+1-10**(dig-1))) sum=sum+1/i; val[k-10**(dig-1)+1]=sum;} 
val=val/(upbound+1-10**(dig-1)); return(val);}}
