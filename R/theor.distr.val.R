theor.distr.val <-
function(lwbound,upbound,dig=1){if (lwbound>upbound) return("lwbound must be less than upbound")
              if (upbound<10**(dig-1)) return("upbound does not have enough digits")
              if (lwbound<(10**(dig-1))) return("lwbound does not have enough digits")
              else {val=rep(0,upbound-lwbound+1); 
for (k in lwbound:upbound) {sum=0; for (i in (k+1-lwbound):(upbound+1-lwbound)) sum=sum+1/i; val[k-lwbound+1]=sum;} 
val=val/(upbound+1-lwbound); return(val);}}
