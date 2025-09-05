addUnits <- function(n, digits = 0) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, digits = digits), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, digits = digits), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, digits = digits), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12, digits = digits), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

format_million <- function(x){
  return(paste0(round(x/1000000, digits=1), "M"))
}