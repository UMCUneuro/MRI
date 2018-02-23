# functions

## TO DO:
## 1. restructure
## 2. write manual


# zelfde als setnames, maar dan voor classes van kolommen
# dt = data.table
# cols = vector met kolomnamen of kolomnummers die je wilt aanpassen
# new_class = vector met classes
setclass2 <- function(dt, cols, new_class){
  if (!is.data.table(dt)) 
    stop("dt is not a data.table")
  if (!length(attr(dt, "names"))) 
    stop("dt has no column names")
  if (length(cols) != length(new_class)) 
    stop("cols is length ", length(cols), " but new_class are length ", 
         length(new_class))
  
  dt0 <- copy(dt)
  dt <- dt[, ..cols]
  dt[] <- lapply(dt, function(x) as.character(x)) #change all classes to character
  for (i in unique(new_class)){
    coln1 <- which(new_class == i)
    class1 <- paste0("as.", i)
    dt[, (coln1) := lapply(.SD, class1), .SDcols = coln1][]
  }
  #merge weer met kolommen die niet in cols voorkwamen
  coln2 <- which(!colnames(dt0) %in% colnames(dt))
  if(length(coln2)>0){
    dt <- cbind(dt, dt0[, coln2, with = FALSE])
  }
  return(dt)
}


# function for count changes
# values for cols:
## NULL             geeft 1 getal terug (over hele vector of dataframe)
## "all"            geeft data.table van 1 rij terug met alle variabelen
## c(colnumbers)    geeft data.table van 1 rij terug met alle gespecificeerde kolomnummers
## c(colnames)      geeft data.table van 1 rij terug met alle gespecificeerde kolomnamen
countNA <- function(data, cols=NULL){
  countNA_internal <- function(x) length(which(is.na(x)))
  
  if(is.null(cols)){
    orig_Nrow <- length(c(t(data)))
    data.table(countNA_internal(data))[,orig_Nrow:=orig_Nrow][]
  }else if(cols[1] == "all"){
    data[, lapply(.SD, function(x) countNA_internal(x))][,orig_Nrow:=nrow(data)][]
  }else{
    data[, lapply(.SD, function(x) countNA_internal(x)), .SDcols = cols][,orig_Nrow:=nrow(data)][]
  }
}
# countNA(bla1, cols = c("T", "W", "disp")) #voorbeeld


# functie om summary over NA's te geven
summaryNA <- function(old, new, count_var = "orig_Nrow", name_data = "data", reason = "..."){
  df1 <- rbind(old, new)
  Nrow <- df1[,get("orig_Nrow")]
  df1[,orig_Nrow:=NULL]
  
  if(length(unique(Nrow))==1){
    df2 <- df1[2,] - df1[1,]
    if(all(unlist(df2)==0)){
      no_change <- data.table(Omschrijving=paste0("Data = ", name_data, "; applied function(s)/modification(s) didn't change the number of NA's."))
      return(no_change)
    }
    df3 <- roundHJ1(df2 / Nrow * 100, digits = 2)
    out1 <- list(variable = colnames(df2), increase_NA = unlist(df2), N = rep(Nrow[1], length(df3)), perc = df3)
    out2 <- setDT(out1)
    
    #gooi alle kolommen met increase_NA==0 weg
    out3 <- out2[increase_NA!=0]
    out3$Omschrijving <- paste0("Data = ", name_data, ", variable = ", out3$variable, "; ", out3$increase_NA, "/",
                                out3$N, " waardes (", out3$perc, "%)", " op NA gezet omdat ", reason)
    return(out3)
  }else{
    stop("Dit nog niet gescript.")
  }
}


#voor non-p-values (functie voor afronden)
roundHJ1 <- function(x, digits=1){
  out1 <- sprintf(paste0("%.", digits, "f"), round(x, digits = digits))
  return(out1)
}


# functie om makkelijker direct uit summaryNA toe te voegen aan mm (ook overzichten in bijv. data.table format)
# good alleen gebruiken als x een vector is
add_mm <- function(x, melding = NULL){
  if(is.null(dim(x))){
    if(is.null(melding)){
      x
    }else{
      paste0(melding, x)
    }
  }else{
    if(is.null(melding)){
      x
    }else{
      list(list(melding), list(x)) #nested list
    }
  }
}


#functie om functies te herkennen in format1
recog_func <- function(x){
  pos1 <- grep("^f\\(.*\\)$", x)
  func1 <- gsub("^f\\(|\\)$", "", x[pos1])
  #func1 <- gsub("^f\\(|\\)$", "", x)
  return(func1)
}
#recog_func(rename1$Maximum)


#functie om minimum en maximum te contoleren
minmax <- function(x, date = FALSE){
  if(length(x)>1){
    stop("x mag momenteel slechts 1 value bevatten")
  }
  if(is.na(x)){
    return(NA)
  }
  
  out1 <- recog_func(x)
  
  if(length(out1)==0){
    out1 <- as.numeric(x)
    if(date == TRUE){
      out1 <- excel_numeric_to_date(out1)
    }
  }else{
    out1 <- eval(parse(text = out1))
  }
  
  return(out1)
}
#minmax(rename1_date$Maximum[1])


#geleend van janitor package, maar geen zin om het hele package te installeren en te laden, dus zo gedaan
excel_numeric_to_date <- function (date_num, date_system = ifelse(MODERN_MAC == TRUE, "modern", "mac pre-2011")) 
{
  if (!is.numeric(date_num)) {
    stop("argument `date_num` must be of class numeric")
  }
  if (date_system == "mac pre-2011") {
    as.Date(date_num, origin = "1904-01-01")
  }
  else if (date_system == "modern") {
    as.Date(date_num, origin = "1899-12-30")
  }
  else {
    stop("argument 'created' must be one of 'mac pre-2011' or 'modern'")
  }
}


# Vind de langste overeenkomende character string
longest_substring <-function(a,b)
{
  A <- strsplit(a, "")[[1]]
  B <- strsplit(b, "")[[1]]
  
  L <- matrix(0, length(A), length(B))
  ones <- which(outer(A, B, "=="), arr.ind = TRUE)
  ones <- ones[order(ones[, 1]), , drop = FALSE] 
  if(length(ones) != 0){
    for(i in 1:nrow(ones)) {
      v <- ones[i, , drop = FALSE]
      L[v] <- ifelse(any(v == 1), 1, L[v - 1] + 1)
    }
    out1 <- paste0(A[(-max(L) + 1):0 + which(L == max(L), arr.ind = TRUE)[1]], # De [1] zorgt ervoor dat de meest linkse match genomen wordt
                   collapse = "")
  } else {
    out1 <- NA_character_
  }
  return(out1)
}


# Wrapper voor functie longest_substring. Vector input en vector/matrix output mogelijk.
longest_substring_vec <- function(a, b = NULL, default = NA_character_, matrix_out = FALSE,
                                  USE.NAMES = FALSE) {
  a <- as.character(a)
  
  if (matrix_out){
    if (is.null(b)) {
      b <- a
    } else {
      b <- as.character(b)
    }
    m <- outer(a, b, Vectorize(longest_substring))
    diag(m) <- NA
    if (USE.NAMES){
      rownames(m) <- a
      colnames(m) <- b
    }
    return(m)
  } else {
    if (is.null(b)) {
      stop("No second input vector given")
    }
    if(length(a) != length(b)){
      stop("Vector lengths do not match")
    }
    m <- mapply(longest_substring, a, b, USE.NAMES = USE.NAMES)
    m[lengths(m) == 0] <- default
    unlist(m)
  }
}


# functie om list van data.tables (ook >2 data.tables) te mergen
# gebruik met Reduce(merge_list, list_of_dt)
merge_list <- function(...) merge(..., all = TRUE)
merge_list_cart <- function(...) merge(..., all = TRUE, allow.cartesian = TRUE)

  