library(GA)

df = read.csv("fanduelList.csv")
batters_ss = read.csv("ss-b.csv")
pitchers_ss = read.csv("ss-p.csv")
batters_rg = read.csv("rg-b.csv")
pitchers_rg = read.csv("rg-p.csv")

split_batters_ss = split(batters_ss, batters_ss$Name)
split_pitchers_ss = split(pitchers_ss, pitchers_ss$Name)
split_batters_rg = split(batters_rg, batters_rg$player)
split_pitchers_rg = split(pitchers_rg, pitchers_rg$player)

split_df = split(df, df$Position)

first_base = split_df[["1B"]]
sec_base = split_df[["2B"]]
third_base = split_df[["3B"]]
pitch = split_df[["P"]]
catch = split_df[["C"]]
of = split_df[["OF"]]
short = split_df[["SS"]]

# format: P / C / 1B / 2B / 3B / SS / OF / OF / OF
max_vector = c(nrow(pitch), nrow(catch), nrow(first_base), 
               nrow(sec_base), nrow(third_base), nrow(short),
               nrow(of), nrow(of), nrow(of))

getNames <- function(player_vector) {
  # Get full names for access in projection dataframe
  player_vector = ceiling(player_vector)
  
  if (!is.matrix(player_vector)) {
    player_vector = matrix(player_vector, ncol=9, byrow = TRUE)
  }
  
  pi = pitch[player_vector[1, 1],]
  ca = catch[player_vector[1, 2],]
  fb = first_base[player_vector[1, 3],]
  sb = sec_base[player_vector[1, 4],]
  tb = third_base[player_vector[1, 5],]
  ss = short[player_vector[1, 6],]
  of1 = of[player_vector[1, 7],]
  of2 = of[player_vector[1, 8],]
  of3 = of[player_vector[1, 9],]
  
  full_names = rep("", 9)
  full_names[1] = paste(pi[["First.Name"]], pi[["Last.Name"]])
  full_names[2] = paste(ca[["First.Name"]], ca[["Last.Name"]])
  full_names[3] = paste(fb[["First.Name"]], fb[["Last.Name"]])
  full_names[4] = paste(sb[["First.Name"]], sb[["Last.Name"]])
  full_names[5] = paste(tb[["First.Name"]], tb[["Last.Name"]])
  full_names[6] = paste(ss[["First.Name"]], ss[["Last.Name"]])
  full_names[7] = paste(of1[["First.Name"]], of1[["Last.Name"]])
  full_names[8] = paste(of2[["First.Name"]], of2[["Last.Name"]])
  full_names[9] = paste(of3[["First.Name"]], of3[["Last.Name"]])
  
  return(full_names)
}

predictedScore <- function(player_vector) {
  player_vector = ceiling(player_vector)
  
  pi = pitch[player_vector[1],]
  ca = catch[player_vector[2],]
  fb = first_base[player_vector[3],]
  sb = sec_base[player_vector[4],]
  tb = third_base[player_vector[5],]
  ss = short[player_vector[6],]
  of1 = of[player_vector[7],]
  of2 = of[player_vector[8],]
  of3 = of[player_vector[9],]
  
  # no repeat players
  if (player_vector[7] == player_vector[8] || player_vector[7] == player_vector[9] 
      || player_vector[8] == player_vector[9]) {
    return(0)
  }
  
  total_salary = pi[["Salary"]] + ca[["Salary"]] + fb[["Salary"]] + sb[["Salary"]] +
    tb[["Salary"]] + ss[["Salary"]] + of1[["Salary"]] + of2[["Salary"]] + of3[["Salary"]]
  
  # exceeding salary = invalid
  if (total_salary >= 35000) {
    return(0) 
  }
  
  # DL = not playing
  if (pi[["Probable.Pitcher"]] != "Yes") {
    return(0)
  }
  
  # Not in batting order = unlikely to play
  batting_orders = c(ca[["Batting.Order"]], fb[["Batting.Order"]],
                     sb[["Batting.Order"]], tb[["Batting.Order"]], 
                     ss[["Batting.Order"]], of1[["Batting.Order"]], 
                     of2[["Batting.Order"]], of3[["Batting.Order"]])
  
  for (i in 1:8) {
    if (!is.na(batting_orders[i])) {
      if (batting_orders[i] == 0) {
        return(0)
      }
    }
  }
  
  # Get full names for access in projection dataframe
  full_names = getNames(player_vector)
  
  
  
  # projected fantasy points = fitness
  proj = 0
  
  for (i in 1:9) {
    if (i == 1) {
      temp = split_pitchers_ss[[full_names[i]]]
      temp2 = split_pitchers_rg[[full_names[i]]]
      if (!is.null(temp)) {
        proj = proj + (temp[["FanDuel"]] / 2)
      }
      if (!is.null(temp2)) {
        proj = proj + (temp2[["fpts"]] / 2)
      }
    } else {
      temp = split_batters_ss[[full_names[i]]]
      temp2 = split_batters_rg[[full_names[i]]]
      if (!is.null(temp)) {
        proj = proj + (temp[["FanDuel"]] / 2)
      }
      if (!is.null(temp2)) {
        proj = proj + (temp2[["fpts"]] / 2)
      }
    }
  }
  
  return(proj)
  
}


for (i in 1:12) {
  solution = ga(type="real-valued", fitness=predictedScore, min=rep(0, 9), max=max_vector, popSize=200, maxiter=150, 
                keepBest=TRUE, parallel=TRUE, pmutation=0.35, elitism=20)
  roster = getNames(solution@solution)
  
  write(solution@fitnessValue, file="output-ss-rg.txt", append=TRUE)
  write(roster, file="output-ss-rg.txt", append=TRUE)
}


