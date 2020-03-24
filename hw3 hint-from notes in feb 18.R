temp=read.table("coal.dat",header=T)
N=dim(temp)[1]
y=year=rep(0,n)
for(i in 1:n){
  y[i]=temp[i,2];
  year[i]=temp[i,1];
}

plot(year,y,xlab="Year",ylab="Number of disasters")