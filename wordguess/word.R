load("vocabulary.RData")

guess=function(vocabulary,easy=1){
#Setting and Initializing
word=NULL;face=NULL;times=ifelse(easy==1,6,8);text=NULL;guess=NULL;j=1
random=function(bank)
{
  k=ceiling(runif(1,0,length(bank)))
  letter=NULL;i=1;
  while(substring(bank[k],i,i)!="")
    {letter[i]=substring(bank[k],i,i);i=i+1}
  return(letter)
}  
word=random(vocabulary)
for(i in 1:length(word))face[i]="_"
print(face,quote=F)
######INPUT
while(times>0){
  flipa=0;flipb=0   
  if("_"%in%face==FALSE){win=1;print("You win!",quote=F);break}
  text[j]=scan(n=1,what="")
  if(text[j]>'z'|text[j]<'a'|nchar(text[j])>1){print("input must be between a and z");next}
  if(j==1)guess[j]=text[j]
  if(j>1){
  if(any(guess[1:j-1]==text[j]))flipb=1    
  #if the letter has already been guessed
  if(flipb==1){{cat(text[j]);cat(" has already been guessed");print(face,quote=F)
                cat("you have guessed: ");cat(text);cat("\n");next}}
    else guess[j]=text[j]}
  for(i in 1:length(word))
    if(guess[j]==word[i]){face[i]=word[i];flipa=1}
  #Correct guess
  if(flipa==0){print("Wrong guess!");times=times-1 } 
    print(face,quote=F)
  cat(times);cat(" times left!");cat("\n")
  cat("you have guessed: ");cat(text);cat("\n")
         j=j+1
}
if(times==0){cat("You lose!The answer is ");print(word,quote=F)}

}