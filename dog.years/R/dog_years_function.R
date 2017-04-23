#' Dog Years
#'
#' This function allows you to know your age in dog years.
#' @param age What is your age?
#' @keywords dog years
#' @export
#' @examples
#' dog_years_function(12)

dog_years_function <- function(age){        #function to input age, return age in medium breed dog years
                                            #source: pets.webmd.com
  if (missing(age)){
    age <- readline(prompt="Enter your age in years: ")
    age <- as.numeric(age)
  }


  if(age<(1/6)){              #calculate age in dog years
     print('You are a 1-week-old Shiba Inu--just a puppy!')
  }

  else if (age<(1/3)){
    print('You are a 2-week-old Shiba Inu--just a puppy!')
  }

  else if(age<(1/2)){
     print('You are a 3-week-old Shiba Inu--just a puppy!')
  }

   else if (age<(2/3)){
     print('You are a 4-week-old Shiba Inu--just a puppy!')
   }

   else if(age<(5/6)){
     print('You are a 5-week-old Shiba Inu--just a puppy!')
   }

   else if (age<(1)){
     print('You are a 6-week-old Shiba Inu--just a puppy!')
   }

   else if (age==1){
     print('You are a 7-week-old Shiba Inu--just a puppy!')
   }

  else if(age>1 && age<15){                     #1yo dog = 15yo human
    dog_age <- .75 + age*.75               #calculate age in dog years
    dog_age <- floor(dog_age)
    sprintf('You are a %i-month-old Shiba Inu--so much dog packed into such a tiny package!', dog_age)    #print result
  }

  else if(age==15){
    dog_age <- 1
    print('You are a 1-year-old Aidi--you finally grew into your paws!', dog_age)    #print result
  }

  else if(age>15 && age<= 24) {             #2yo dog = 24yo human
    dog_age <- 1 + (age-15)/9
    dog_age <- ceiling(dog_age)
    sprintf('You are a %i-year-old labradoodle--no leash can hold you!', dog_age)
  }

  else if(age>24 && age<= 36){            #3yo dog = 28yo human
    dog_age <- 2 + (age-24)/4             #5yo dog = 32yo human: dogs age 4yrs/yr b/t 3 and 5
    dog_age <- ceiling(dog_age)
    sprintf('You are a %i-year-old Rottweiler--the world is your chew toy!', dog_age)
  }

  else if(age>36 && age<= 75) {           #6yo dog = 42yo human; 13.25yo dog = 75 yo human
    dog_age <- 5 + (age-36)/4.5           #dogs age 4.5yrs/yr after age 5
    dog_age <- ceiling(dog_age)
      if (dog_age < 12){
        sprintf('You are a %i-year-old Austrailian Cattle Dog--prime time for a working breed!', dog_age)
      }
      else if (dog_age>=12){
        sprintf('You are a %i-year-old Schnauzer--a Professor Emeritus of Pupology!', dog_age)
      }
  }

  else {                                  #I made this part up
    dog_age <- 16 + (age-75)/4
    dog_age <- ceiling(dog_age)
    sprintf('You are a %i-year-old New Guinea Singing Dog--your unique howl conveys the richness of your experience!', dog_age)
  }
}
