(* Exercise 2.7
   Author: Cyrus Ramavarapu
   Purpose: Subtract triplets of old english
   money
    
   12 pence = 1 shilling
   20 shilling = 1 pound
*)

fun check_negatives (pounds, shillings, pence) = 
    if pounds >= 0 andalso shillings >= 0 andalso pence >= 0 then (pounds, shillings, pence)
    else if pence < 0 then check_negatives(pounds, shillings-1, pence+12) 
    else if shillings < 0 then check_negatives(pounds-1, shillings+20, pence) 
    else (* pounds < 0  *) (0,0,0); 


fun subtract_money((pounds_1, shilling_1, pence_1),(pounds_2, shilling_2, pence_2)) = 
    check_negatives(pounds_1-pounds_2, shilling_1-shilling_2, pence_1-pence_2);

print("test 1 -- equal amounts\n");
subtract_money((1,1,1),(1,1,1));
print("test 2 -- 1 pence change\n");
subtract_money((1,1,1),(1,1,0));
print("test 3 -- 1 shilling change\n");
subtract_money((1,1,1),(1,0,1));
print("test 3 -- 1 pound change\n");
subtract_money((1,1,1),(0,1,1));
print("test 4 -- carry shillings\n");
subtract_money((0,1,1),(0,0,2));
print("test 5 -- carry pound for pence\n");
subtract_money((1,0,1),(0,0,2));
print("test 6 -- carry pound for shilling\n");
subtract_money((1,1,0),(0,2,0));
print("test 7 -- negative difference\n");
subtract_money((1,1,0),(1,2,0));
