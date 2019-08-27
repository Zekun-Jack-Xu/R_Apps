game=function(){
  deck=NULL;  deck$n=rep(1:13,4);  deck$k=rep(c("spade","heart","club","diamond"),13)
  cards=NULL; computer=NULL; player=NULL;computeranswer=0;playeranswer=0
  computercount=0;playercount=0
  random=NULL;random=sample(1:52,52,replace=F)
  for (i in 1:52) {cards$n[i]=deck$n[random[i]];cards$k[i]=deck$k[random[i]]}
  
  for (i in 1:52) {if (cards$n[i]==1) cards$face[i]="A"
                   else if(cards$n[i]==11) cards$face[i]="J"
                   else if(cards$n[i]==12) cards$face[i]="Q"
                   else if(cards$n[i]==13) cards$face[i]="K"
                   else cards$face[i]=cards$n[i]
  }
  
  for (i in 1:2) {
                  computer$n[i]=cards$n[2*i-1];computer$face[i]=cards$face[2*i-1];computer$k[i]=cards$k[2*i-1]
                  player$n[i]=cards$n[2*i];player$face[i]=cards$face[2*i];player$k[i]=cards$k[2*i]}
  
  print("Now begins the game! Playing against Jack in BlackJack!")
  print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat("\n")
  print("Jack's first card is");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat("\n");cat("Jack's second card is a secret :)");cat("\n")
  
  #computer's decision
    for (i in 1:2) {if(computer$n[i]>=10) computer$value[i]=10
                  else if(computer$n[i]==1)  computer$value[i]=11
                    else computer$value[i]=computer$n[i]
                    if(player$n[i]>=10) player$value[i]=10
                    else if(player$n[i]==1)  player$value[i]=11
                    else player$value[i]=player$n[i]  
                      }
  computer$sum=sum(computer$value)
  if(computer$n[1]==1 & computer$n[2]==1) computer$sum=12
  player$sum=sum(player$value)
  if(player$n[1]==1 & player$n[2]==1) player$sum=12
  if (computer$sum<player$sum | computer$sum<16 ) computeranswer="y"
  else computeranswer="n"
  
  while(1)
  {print("Do you want a third card? y/n")
   playeranswer=scan(n=1,what="")
   if (playeranswer=="y" | computeranswer=="n") break}
   
  if(playeranswer=="n" & computeranswer=="n")
      {if (computer$sum>player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat("\n")
                                   print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat("\n")
                                   print("Sorry, you lose...But don't be sad, cos Jack is awesome~")}
      else if (computer$sum==player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat("\n")
                                          print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat("\n")
                                        print("Let's call it a tie~ Jack strongly invites you to play again!")}
       else if (computer$sum<player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat("\n")
                                     print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat("\n")
                                     print("You win! Impressive! It's been long since Jack lost his last game! Jack owes you a favor~")}
       }
  if(playeranswer=="n" &computeranswer=="y")
  {computer$n[3]=cards$n[5];computer$k[3]=cards$k[5];computer$face[3]=cards$face[5]
   for(i in 1:3) {if (computer$n[i]==1) computercount=computercount+1
                  if(computer$n[i]>=10) computer$value[i]=10
                  else computer$value[i]=computer$n[i]                  
                  }
   if(computercount==3) computer$sum=13
   else if(computercount==0) computer$sum=sum(computer$value)
   else if(computercount==1|comptercount==2) {if(21-sum(computer$value)>=10) computer$sum=sum(computer$value)+10
                              else computer$sum=sum(computer$value)}
  if(computer$sum>21 | computer$sum<player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat("\n")
                       print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat(" , ");cat(computer$k[3]);cat(" ");cat(computer$face[3]);cat("\n")
                       print("You win! Impressive! It's been long since Jack lost his last game! Jack owes you a favor~")}
  else if (computer$sum>player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat("\n")
                                 print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat(" , ");cat(computer$k[3]);cat(" ");cat(computer$face[3]);cat("\n")
                                 print("Sorry, you lose...But don't be sad, cos Jack is awesome~")}
   else if (computer$sum==player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat("\n")
                                       print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat(" , ");cat(computer$k[3]);cat(" ");cat(computer$face[3]);cat("\n")
                                       print("Let's call it a tie~ Jack strongly invites you to play again!")}
   }
  if(playeranswer=="y" &computeranswer=="n")
  {player$n[3]=cards$n[5];player$k[3]=cards$k[5];player$face[3]=cards$face[5]
   for(i in 1:3) {if (player$n[i]==1) playercount=playercount+1
                  if(player$n[i]>=10) player$value[i]=10
                  else player$value[i]=player$n[i]                  
   }
   if(playercount==3) player$sum=13
   else if(playercount==0) player$sum=sum(player$value)
   else if(playercount==1|playercount==2) {if(21-sum(player$value)>=10) player$sum=sum(player$value)+10
                                              else player$sum=sum(player$value)}
   if(player$sum<=21 | computer$sum<player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat(" , ");cat(player$k[3]);cat(" ");cat(player$face[3]);cat("\n")
                                                  print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat("\n")
                                                  print("You win! Impressive! It's been long since Jack lost his last game! Jack owes you a favor~")}
   else if (computer$sum>player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat(" , ");cat(player$k[3]);cat(" ");cat(player$face[3]);cat("\n")
                                      print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat("\n")
                                      print("Sorry, you lose...But don't be sad, cos Jack is awesome~")}
   else if (computer$sum==player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat(" , ");cat(player$k[3]);cat(" ");cat(player$face[3]);cat("\n")
                                       print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat("\n")
                                       print("Let's call it a tie~ Jack strongly invites you to play again!")}
  }
  if(playeranswer=="y" &computeranswer=="y")
  {computer$n[3]=cards$n[5];computer$k[3]=cards$k[5];computer$face[3]=cards$face[5]
   player$n[3]=cards$n[6];player$k[3]=cards$k[6];player$face[3]=cards$face[6]
   for(i in 1:3) {if (computer$n[i]==1) computercount=computercount+1
                  if(computer$n[i]>=10) computer$value[i]=10
                  else computer$value[i]=computer$n[i]     
                  if (player$n[i]==1) playercount=playercount+1
                  if(player$n[i]>=10) player$value[i]=10
                  else player$value[i]=player$n[i]
   }
   if(computercount==3) computer$sum=13
   else if(computercount==0) computer$sum=sum(computer$value)
   else if(computercount==1|comptercount==2) {if(21-sum(computer$value)>=10) computer$sum=sum(computer$value)+10
                                              else computer$sum=sum(computer$value)}
   if(playercount==3) player$sum=13
   else if(playercount==0) player$sum=sum(player$value)
   else if(playercount==1|playercount==2) {if(21-sum(player$value)>=10) player$sum=sum(player$value)+10
                                           else player$sum=sum(player$value)}
   
   if(player$sum<=21 & computer$sum<player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat(" , ");cat(player$k[3]);cat(" ");cat(player$face[3]);cat("\n")
                                                  print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat(" , ");cat(computer$k[3]);cat(" ");cat(computer$face[3]);cat("\n")
                                                  print("You win! Impressive! It's been long since Jack lost his last game! Jack owes you a favor~")}
   else if (computer$sum<=21 & computer$sum>player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat(" , ");cat(player$k[3]);cat(" ");cat(player$face[3]);cat("\n")
                                      print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat(" , ");cat(computer$k[3]);cat(" ");cat(computer$face[3]);cat("\n")
                                      print("Sorry, you lose...But don't be sad, cos Jack is awesome~")}
   else if (computer$sum==player$sum) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat(" , ");cat(player$k[3]);cat(" ");cat(player$face[3]);cat("\n")
                                       print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat(" , ");cat(computer$k[3]);cat(" ");cat(computer$face[3]);cat("\n")
                                       print("Let's call it a tie~ Jack strongly invites you to play again!")}
   else if (computer$sum>21 & player$sum>21) {print("Your cards are ");cat(player$k[1]);cat(" ");cat(player$face[1]);cat(" , ");cat(player$k[2]);cat(" ");cat(player$face[2]);cat(" , ");cat(player$k[3]);cat(" ");cat(player$face[3]);cat("\n")
                                       print("Jack's cards are ");cat(computer$k[1]);cat(" ");cat(computer$face[1]);cat(" , ");cat(computer$k[2]);cat(" ");cat(computer$face[2]);cat(" , ");cat(computer$k[3]);cat(" ");cat(computer$face[3]);cat("\n")
                                       print("Let's call it a tie~ Jack strongly invites you to play again!")}
  }
  }