## Prajwal Bhandari STAT184 Activity 03 Sep 2024


get_c <- function(length, width){
  # skipped a bunch of preliminary math, but these are the roots to V' = 0
  c_1 <- ((length + width) + sqrt(length^2 - length*width + width^2))/6
  c_2 <- ((length + width) - sqrt(length^2 - length*width + width^2))/6

  if (c_1 > (min(length, width)/2)) {
    print("c1 does not work")
  }
  if (c_2 > (min(length, width)/2)) {
    print('c2 does not work')
  }

  # check which c value gets the largest volume
  vol_c1 <- (length - 2*c_1)*(width - 2*c_1)*c_1
  print(vol_c1)
  vol_c2 <- (length - 2*c_2)*(width - 2*c_2)*c_2
  print(vol_c2)

  ifelse(vol_c1 > vol_c2, return(c_1), return(c_2))
  }

print(get_c(11,8.5))


vol <- function(c_arr, height, width){
  return(c_arr*(height - 2*c_arr)*(width - 2*c_arr))
}

c_arr = seq(from = 1, to = 3, by = 0.25)

plot(
  x = c_arr,
  y = vol(c_arr, 8.5, 11),
  type = 'l'
)

#treat 8.5,11 as l,w
vol_from_c <- function(c){
  return(c*(8.5 - 2*c)*(11 - 2*c))
}



collatz <- function(n){
  count = 0
  if (n == 1) {
    count = count + 1
  }
  while (n > 1) {
    if (n %% 2 == 0) {
      count = count + 1
      n = n/2
    } else {
      count = count + 1
      n = 3*n + 1
    }
  }
  return(count)
}

first_20000 <- seq(1, 20000, 1)
collatz_list <- sapply(first_20000,`collatz`)
hist(
  collatz_list,
  xlab = "number of steps",
  ylab = 'frequency')

sim_spins <- function(win_condition){
  spinner = c('joni', 'salome', 'joni', 'joni', 'salome', 'joni', 'joni','salome')
  p1_wins = 0  # joni
  p2_wins = 0  # salome
  counter = 0
  while (p1_wins < win_condition & p2_wins < win_condition) {
    if (counter %% 2 == 0) {
      spin <- sample(spinner,1)
      if (spin == 'joni') {
        p1_wins <- p1_wins + 1
      }
      } else {
        spin <- sample(spinner, 1)
        if (spin == 'salome') {
          p2_wins <- p2_wins + 1
        }
      }
    counter = counter + 1
  }
  if (p1_wins == win_condition) {
    return("joni")
  } else {
    return("salome")
  }
}


simResults <- replicate(
  n = 100000,
  expr = sim_spins(1)
)
table(simResults)

