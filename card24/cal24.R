cal=function(a,b,c,d)
{
  options(digits=3)
  answer="N/A"
if(a+b+c+d==24) 
  {answer=paste(a,"+",b,"+",c,"+",d,"=",24,sep="")}
else if(a+b-c-d==24)
  {answer=paste(a,"+",b,"-",c,"-",d,"=",24,sep="")}
else if(abs(a/(b-c/d)-24)<0.0001)
  {answer=paste("(",a,"/(",b,"-",c,"/",d,"))=",24,sep="")}
else if (a+b+c-d==24)
  {answer=paste(a,"+",b,"+",c,"-",d,"=",24,sep="")}
else if((a+b)*(c+d)==24)
  {answer=paste("(",a,"+",b,")*(",c,"+",d,")=",24,sep="")}
else if ((a+b)*(c-d)==24)
  {answer=paste("(",a,"+",b,")*(",c,"-",d,")=",24,sep="")}
else if ((a-b)*(c-d)==24)
  {answer=paste("(",a,"-",b,")*(",c,"-",d,")=",24,sep="")}
else if((a+b+c)*d==24)
  {answer=paste("(",a,"+",b,"+",c,")*",d,"=",24,sep="")}
else if((a-b-c)*d==24)
  {answer=paste("(",a,"-",b,"-",c,")*",d,"=",24,sep="")}
else if((a+b-c)*d==24)
  {answer=paste("(",a,"+",b,"-",c,")*",d,"=",24,sep="")}
else if(a*b*c*d==24)
  {answer=paste(a,"*",b,"*",c,"*",d,"=",24,sep="")}
else if(a*b*c/d==24)
  {answer=paste(a,"*",b,"*",c,"/",d,"=",24,sep="")}
else if(a*b+c+d==24)
  {answer=paste(a,"*",b,"+",c,"+",d,"=",24,sep="")}
else if(a*b*(c+d)==24)
  {answer=paste(a,"*",b,"*(",c,"+",d,")=",24,sep="")}
else if(a*b*(c-d)==24)
  {answer=paste(a,"*",b,"*(",c,"-",d,")=",24,sep="")}
else if(a*b*c-d==24)
  {answer=paste(a,"*",b,"*",c,"-",d,"=",24,sep="")}
else if(a*b*c+d==24)
  {answer=paste(a,"*",b,"*",c,"+",d,"=",24,sep="")}
else if(a+b+c/d==24)
  {answer=paste(a,"+",b,"+",c,"/",d,"=",24,sep="")}
else if((a+b)*c/d==24)
  {answer=paste("(",a,"+",b,")*",c,"/",d,"=",24,sep="")}
else if((a*b+c)/d==24)
  {answer=paste("(",a,"*",b,"+",c,")/",d,"=",24,sep="")}
else if(a*b+c+d==24)
  {answer=paste(a,"*",b,"+",c,"+",d,"=",24,sep="")}
else if((a*b-c)/d==24)
  {answer=paste("(",a,"*",b,"-",c,")/",d,"=24",sep="")}
else if(a*b+c-d==24)
  {answer=paste(a,"*",b,"+",c,"-",d,"=",24,sep="")}
else if(a*b-c/d==24)
  {answer=paste(a,"*",b,"-",c,"/",d,"=",24,sep="")}
else if(a*b+c/d==24)
  {answer=paste(a,"*",b,"+",c,"/",d,"=",24,sep="")}
else if(a*b-c-d==24)
  {answer=paste(a,"*",b,"-",c,"-",d,"=",24,sep="")}
else if(a*b+c*d==24)
  {answer=paste(a,"*",b,"+",c,"*",d,"=",24,sep="")}
else if(a*b-c*d==24)
  {answer=paste(a,"*",b,"-",c,"*",d,"=",24,sep="")}
else if((a*b)/(c*d)==24)
  {answer=paste("(",a,"*",b,")/(",c,"*",d,")=",24,sep="")}
else if(a*b/(c-d)==24)
  {answer=paste(a,"*",b,"/(",c,"-",d,")=",24,sep="")}
else if(a*b/(c+d)==24)
  {answer=paste(a,"*",b,"/(",c,"+",d,")=",24,sep="")}
else if(a*(b-c/d)==24)
  {answer=paste(a,"*(",b,"-",c,"/",d,")=",24,sep="")}
return(answer)
}
