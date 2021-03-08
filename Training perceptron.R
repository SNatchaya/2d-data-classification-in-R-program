#step function
step_func = function(value, threshold)
{
	if(value>=threshold)
		result = 1
	else
		result = 0
	result	
}

#train perceptron
perceptron = function(x)
{	
	err = 1
	iter = 0
	maxi = length(x$x1)
	while(err>0.01 && iter<5000)
	{
	  w1 = runif(1)	#RANDOM VALUE		
	  w2 = runif(1)	#RANDOM VALUE
	  
	  err = 0
	  for(i in 1:maxi)
	  {
	    x1 = x[i,1]
	    x2 = x[i,2]
	    sum = x1*w1 + x2*w2
	    
	    yh = step_func(sum,100)
	    yt = x[i,3]
	    
	    if(yh!=yt)
	      err = err+1
	  }
	  err = err/maxi
	  iter = iter+1
	}
	listReturn = list("weight" = c(w1,w2), "error" = err)
	return(listReturn)
} 

#test perceptron
test_perceptron = function(x,w1,w2)
{
	err = 1
	iter = 0
	maxi = length(x$x1)
	err = 0
	for(i in 1:maxi)
	{
		x1 = x[i,1]
		x2 = x[i,2]
		sum = x1*w1 + x2*w2
		
		yh = step_func(sum,100)
		yt = x[i,3]
		
		if(yh==yt)
		{
			print("pass")	
		}
		else
		{
			print("not pass")
		  print(sprintf("x1 = %f , x2 = %f",x1,x2))
		  lines(x1,x2,type="b",col="green")
			err = err+1
		}
	}
	print(err/maxi)
} 


#Visualize all data

print("----START-----")
#LOAD DATA
ga = read.csv("group_a.txt", header=T, sep="\t")
gb = read.csv("group_b.txt", header=T, sep="\t")

#PLOT DATA
plot.new()
plot(ga[,1:2],col="red")
lines(gb[,1:2],type="p",col="blue")
legend("topleft", legend=c("group a","group b","test fail"),
       col=c("red","blue","green"), lty = 1:2, cex=0.6)

#train data
print("-----TRAIN-----")
gc = rbind(ga[1:4,],gb[1:4,])	#SMALL
#gc = rbind(ga,gb) 				#LARGE
output = perceptron(gc)
print(output)

#test data
print("-----TEST------")
gc = rbind(ga,gb) 				#LARGE
test_perceptron(gc,output$weight[1],output$weight[2])
print("----END-----")