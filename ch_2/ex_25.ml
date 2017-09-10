(*
 * Purpose: A function that checks if the 
            input values form a valid date.
 * Author: Cyrus Ramavarapu
 * Date: 23 July 2017
 *)

fun check_date d m = 
    if d <= 0 orelse d > 31 then false
    else
        if m = "January" andalso d <= 31 then true
        else if m = "February" andalso d <= 28 then true
        else if m = "March" andalso d <= 31 then true
        else if m = "April" andalso d <= 30 then true
        else if m = "May" andalso d <= 31 then true
        else if m = "June" andalso d <= 30 then true
        else if m = "July" andalso d <= 31 then true
        else if m = "August" andalso d <= 31 then true
        else if m = "September" andalso d <= 30 then true
        else if m = "October" andalso d <= 31 then true
        else if m = "November" andalso d <= 30 then true
        else if m = "December" andalso d <= 31 then true
        else (* Invalid month *) false;
    ;

        
