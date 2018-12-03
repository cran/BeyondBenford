Blon.val <-
function(upbound,fig,dig=1){if (dig==1) 
	{if (fig>upbound) return(0) 
	else {k=0; while ((fig*10**k)<=upbound) k=k+1; u=1; v=(fig-1); S=0; T=0; 
		if (k>=2) {for (i in 0:(k-2)) {for (b in (fig*10**i):((fig+1)*10**i-1)) T=T+(b-v)/b; 
			for (a in ((fig+1)*10**i):(fig*10**(i+1)-1)) S=S+u/a; u=u+10**(i+1); v=v*10+8;}}; 
		if (upbound<((fig+1)*10**(k-1))) {for (b in (fig*10**(k-1)):upbound) T=T+(b-v)/b;} 
		else {for (b in (fig*10**(k-1)):((fig+1)*10**(k-1)-1)) T=T+(b-v)/b; for (a in ((fig+1)*10**(k-1)):upbound) S=S+u/a;}; 
	return((S+T)/upbound)}}
	else {if (upbound<10**(dig-1)) print("upbound does not have enough digits")
	else{k=-1; while(10**(k+dig+1)<=upbound) k=k+1; l=floor((upbound-(10**(dig-1)+fig)*10**(k+1))/10**(k+2))+10**(dig-2); S=0;T=0; 
    if (k!=-1){
        for (i in 0:k) {
            for (j in (10**(dig-2)):(10**(dig-1)-1)) {
                for (b in ((10*j+fig)*10**i):((10*j+(fig+1))*10**i-1))
                    T=T+(b-((9*j+fig)*10**i+10**(dig-2)-1))/(b+1-10**(dig-1));}
            for (j in (10**(dig-2)-1):(10**(dig-1)-1)) {
		if (max(10**(dig+i-1),(10*j+(fig+1))*10**i)<=min(10**(dig+i)-1,(10*(j+1)+fig)*10**i-1)) {
                for (a in max(10**(dig+i-1),(10*j+(fig+1))*10**i):min(10**(dig+i)-1,(10*(j+1)+fig)*10**i-1))
                    S=S+((j+1)*10**i-10**(dig-2))/(a+1-10**(dig-1)); }}}}
    if ((floor(upbound/10**(k+1))-10*floor(upbound/10**(k+2)))==fig & l>=10**(dig-2)) {
        for (j in 10**(dig-2):l) {
            for (b in ((10*j+fig)*10**(k+1)):min(upbound,(10*j+(fig+1))*10**(k+1)-1))
                T=T+(b-((9*j+fig)*10**(k+1)+10**(dig-2)-1))/(b+1-10**(dig-1));}
        for (j in (10**(dig-2)-1):(l-1)){
            if (max(10**(dig+k),(10*j+(fig+1))*10**(k+1))<=((10*(j+1)+fig)*10**(k+1)-1)) 
		{for (a in max(10**(dig+k),(10*j+(fig+1))*10**(k+1)):((10*(j+1)+fig)*10**(k+1)-1))
                S=S+((j+1)*10**(k+1)-10**(dig-2))/(a+1-10**(dig-1));}}}
    else {
        if (l>=10**(dig-2)) {for (j in 10**(dig-2):l){
            for (b in ((10*j+fig)*10**(k+1)):((10*j+(fig+1))*10**(k+1)-1))
                T=T+(b-((9*j+fig)*10**(k+1)+10**(dig-2)-1))/(b+1-10**(dig-1));}}
        if (l>=10**(dig-2)-1) {for (j in (10**(dig-2)-1):l){
		if (max(10**(dig+k),(10*j+(fig+1))*10**(k+1))<=min(upbound,(10*(j+1)+fig)*10**(k+1)-1)) {
            	for (a in max(10**(dig+k),(10*j+(fig+1))*10**(k+1)):min(upbound,(10*(j+1)+fig)*10**(k+1)-1))
                S=S+((j+1)*10**(k+1)-10**(dig-2))/(a+1-10**(dig-1));}}}}
    return((S+T)/(upbound+1-10**(dig-1)))}}}
