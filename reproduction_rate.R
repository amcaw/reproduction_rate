## required packages
library(EpiEstim)
library(knitr)

## import data
dta <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv")

## aggregate new intakes
dta_agg <- aggregate(NEW_IN ~ DATE, dta, sum)
names(dta_agg) <- c("dates", "I")
dta_agg$dates <- as.Date(dta_agg$dates)

## calculate effective reproduction number
Rt <-
estimate_R(
  dta_agg,
  method = "parametric_si",
  config = make_config(list(mean_si = 4.7, std_si = 2.9)))

## tabulate last values
collapse <-
function(x) {
  x <- formatC(x, format = "f", digits = 2)
  paste0(x[1])
}

zz<-kable(
  col.names = c("DATE", "Rt_calc"),
  data.frame(
    DATE = tail(unique(dta$DATE), 14),
    Rt = apply(tail(Rt$R[, c(8, 5, 11)], 14), 1, collapse)))

write.csv(data.frame(
    DATE = tail(unique(dta$DATE), 14),
    Rt = apply(tail(Rt$R[, c(8, 5, 11)], 14), 1, collapse)), "main/reproduction_rate/data/result.csv")
