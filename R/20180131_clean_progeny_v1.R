# clean data Progeny
# version 0.1
# date: 31 January 2018

# TODO:
# 1. testhat inbouwen (issue voor gemaakt)
# 2. order in longitudinale data maken (nadat dates gecontroleerd zijn).


######################
##  ARCHITECTURE  ####
######################

# to complete #############-
# 1.  rename
# 2.  reclassify
# 3.  check minimum
# 4.  check maximum
# 5.  check possible values
# 6.  check regex
# 7.  cross vs long data
# 8.  wide to long format
# 9.  sumscores ALSFRS-R (tzt verder naar beneden plaatsen)
# 10. check dates
# 11. definite split of cross and long data


####################
####  SETTINGS  ####
####################

# package dir
#DIR1 <- "/Volumes/Samsung_T1/Vakantie/HJ/Imaging/R_packages/MRI" # HJ
DIR1 <- "/Users/htan4/Documents/Rprojects/MRI" # Harold

# source settings
source(paste0(DIR1, "/R/settings.R"))


#####################
####  FUNCTIONS  ####
#####################

# source functions
source(paste0(DIR1, "/R/functions.R"))


#######################
####  DEFINITIONS  ####
#######################

# source definitions
source(paste0(DIR1, "/R/definitions.R"))


################################
####  LOAD DATA & PACKAGES  ####
################################

# load packages
library(readxl)
library(data.table)
library(tidyr)
# additionally needed packages: igraph

# load data
d1 <- data.table(read_excel(path = paste0(DIR1, "/Progeny/20180221_progeny.xlsx"), col_types = "text"))
format1 <- data.table(read_excel(path = paste0(DIR1, "/Data/Format_v1.xlsx"), sheet = 1))
dep1 <- data.table(read_excel(path = paste0(DIR1, "/Data/Format_v1.xlsx"), sheet = 2))


##########################
####  PREFORMAT DATA  ####
##########################

# maak copy van d1
d2 <- copy(d1)

# detele in format1 alle rijen waarin 1 # staat
format2 <- format1[ifelse(rowSums(format1 == "#", na.rm = TRUE)>0, FALSE, TRUE),]

# zet Rename van identifier op ALSnr
format2[Group=="ID", Rename:="ALSnr"]

#maak long format van dependencies (sheet 2 in format1.xlsx)
dep2 <- copy(dep1)
colnames(dep2)[1] <- "to"
dep2 <- data.table(na.omit(gather(dep2, from, value, -to)))
dep2 <- dep2[, .(from, to, value)]
dep2$uitleg <- ifelse(dep2$value == -1, "'from' komt eerder dan 'to'.",
                      ifelse(dep2$value == 1, "'from' komt later dan 'to'.", "FOUT??"))

# maak meldingen file
mm <- list("MELDINGEN")

# check of alle ID's uniek zijn
uniq1 <- na.omit(d2[, format2[Group=="ID"]$Original, with = FALSE])
if(!identical(nrow(uniq1), nrow(unique(uniq1)))){
  stop("Identifier is niet uniek!! Oplossen voordat je verder gaat.")
}else{
  mm <- c(mm, "GOED: Identifier (ALSnr) is uniek.")
}


##########################
####  DATA WRANGLING  ####
##########################

##########################
####  RENAME COLUMNS  ####
##########################

#check whether all columnnames in d1 exist in format2.
not_in_format2 <- colnames(d1)[!colnames(d1) %in% format2$Original]
if(length(not_in_format2)>0){
  mm <- c(mm, paste0("MOGELIJKE FOUT: ", paste(not_in_format2, collapse = ", "),
                     if(length(not_in_format2)>1) " komen" else " komt",
                     " niet in format2 voor, maar wel in d1. Deze variabelen hebben hun originele naam gehouden."))
  
}else{
  mm <- c(mm, "GOED: Alle variabelen in d1 komen ook voor in d1.")
}

#rename colnames
rename1 <- merge(data.table(Original = colnames(d2), pos = 1:ncol(d2)), format2, by = "Original", all.x = TRUE)
setkey(rename1, pos)
setnames(d2, old = rename1[!is.na(Rename)]$Original, new = rename1[!is.na(Rename)]$Rename)
mm <- c(mm, "GOED: kolomnamen in d2 aangepast (conform format2).")


#######################
####  ADJUST CLASS ####
#######################

# adjust class
old1 <- countNA(d2, cols = "all")
d2 <- setclass2(d2, cols = rename1[!is.na(Class)]$Rename, new_class = rename1[!is.na(Class)]$Class)
new1 <- countNA(d2, cols = "all")

# melding over adjust class in mm
summary1 <- summaryNA(old = old1, new = new1, count_var = "orig_Nrow", name_data = "d2",
                      reason = "de opgelegde dataclass niet overeenkwam met de werkelijke class (=FOUT)")
melding1 <- ifelse(is.vector(summary1), "GOED: ",
                   "FOUT: hieronder een overzicht met variabelen waarvan het aantal NA's veranderde door de class aan te passen (Data = d2)")
mm <- c(mm, add_mm(x = summary1, melding = melding1))


#####################################
####  CHECK MINIMUM AND MAXIMUM  ####
#####################################

#split rename1 in date en nondate
rename1_date1 <- rename1[Class=="Date"]
rename1_nondate1 <- rename1[Class!="Date"]

#controleer minimum en maximum voor non-dates
old1 <- countNA(d2, cols = "all")
for (x in 1:nrow(rename1_nondate1)){
  min1 <- minmax(rename1_nondate1$Minimum[x])
  max1 <- minmax(rename1_nondate1$Maximum[x])
  if(!is.na(min1)){
    set(d2, i = which(d2[[rename1_nondate1$Rename[x]]] < min1), j = rename1_nondate1$Rename[x], value = NA)
  }
  if(!is.na(max1)){
    set(d2, i = which(d2[[rename1_nondate1$Rename[x]]] > max1), j = rename1_nondate1$Rename[x], value = NA)
  }
}
new1 <- countNA(d2, cols = "all")
reason1 <- "de waarde van deze variabele(n) niet tussen de gedefinieerde minimale en maximale waarden lag."
mm <- c(mm, list(summaryNA(old1, new1, name_data = "d2", reason = reason1)))

#controleer minimum en maximum voor dates
old1 <- countNA(d2, cols = "all")
for (x in 1:nrow(rename1_date1)){
  min1 <- minmax(rename1_date1$Minimum[x], date = TRUE)
  max1 <- minmax(rename1_date1$Maximum[x], date = TRUE)
  if(!is.na(min1)){
    set(d2, i = which(d2[[rename1_date1$Rename[x]]] < min1), j = rename1_date1$Rename[x], value = NA)
  }
  if(!is.na(max1)){
    set(d2, i = which(d2[[rename1_date1$Rename[x]]] > max1), j = rename1_date1$Rename[x], value = NA)
  }
}
new1 <- countNA(d2, cols = "all")
mm <- c(mm, list(summaryNA(old1, new1, name_data = "d2", reason = reason1)))


#################################
####  CHECK POSSIBLE VALUES  ####
#################################

# check possible values
val1 <- rename1[!is.na(Possible_values)]
val2 <- strsplit(val1$Possible_values, split = "\\|")
old1 <- countNA(d2, cols = "all")
for (x in 1:length(val2)){
  set(d2, i = which(!d2[[val1$Rename[x]]] %in% val2[[x]]), j = val1$Rename[x], value = NA)
}
new1 <- countNA(d2, cols = "all")
reason1 <- "de waarde van deze variabele(n) niet voorkwam in de 'possible values'."
mm <- c(mm, list(summaryNA(old1, new1, name_data = "d2", reason = reason1)))


########################
####  CHECK  REGEX  ####
########################

# vul eerst even alle ALSnrs (=ID)
#  ik ga er hierbij van uit dat ALSnr nooit missing is (en zo is het ook ingesteld in de query van 20180130_MRI1)
ID1 <- rename1[Group=="ID"]
if(nrow(ID1)!=1){
  stop("Er is >1 of <1 Identifier aangegeven in rename1")
}
d2[, ALSnr_NA:=is.na(ALSnr)]
d2 <- fill(d2, ALSnr)

# verwijder rijen waarvan ALSnr niet aan Regex voldoet (indien Regex aanwezig is)
if(!is.na(ID1$Regex)){
  #old1 <- countNA(d2, cols = "ALSnr")
  old1 <- nrow(d2)
  d2 <- d2[grepl(ID1$Regex, ALSnr)]
  d2[, ALSnr:=factor(ALSnr)] # ik ga er hier voor het gemak even vanuit dat de identifier altijd factor is
  new1 <- nrow(d2)
  mm <- c(mm, paste0("In d2, ", old1-new1, "/", old1, " (", roundHJ1((old1-new1)/old1*100, 2), "%)",
                     " rijen verwijderd omdat identifier (i.e. ALSnr) niet voldeed aan de opgegeven regex."))
  #new1 <- countNA(d2, cols = "ALSnr")
}

# check overige regex (indien aanwezig)
regex1 <- rename1[!is.na(Regex)][Rename!="ALSnr"]
if(nrow(regex1)>0){
  old1 <-countNA(d2, cols = "all")
  for (x in 1:nrow(regex1)){
    val1 <- d2[,get(regex1$Rename[x])]
    set(d2, i = grep(regex1$Regex[x], val1, invert = TRUE), j = regex1$Rename[x], value = NA)
  }
  new1 <- countNA(d2, cols = "all")
  reason1 <- "de waarde van deze variabele(n) niet voorkwam in de gespecificeerde 'regex' (zie format1)."
  mm <- c(mm, list(summaryNA(old1, new1, name_data = "d2", reason = reason1)))
}


####################################
####  CHECK LONG vs CROSS DATA  ####
####################################

# maak overzicht met long data
long1 <- d2[ALSnr_NA==TRUE, ] #potential long dataset
long2 <- long1[, colSums(!is.na(long1)) > 0, with = FALSE] # long dataset

# maak in long format groepen van data die bij elkaar hoort
a1 <- which(!colnames(long2) %in% c("ALSnr", "ALSnr_NA"))
l1 <- lapply(a1, function(x){
  out1 <- which(!is.na(long2[, ..x]))
  return(out1)
})
com1a <- apply(combn(seq_along(l1), 2), 2, function(n) l1[[n[1]]] %in% l1[[n[2]]])
com1b <- combn(seq_along(l1), 2)
com1c <- com1b[,which(unlist(lapply(com1a, function(i){length(which(i==TRUE))}))>0)]

# gebruik even igraph om connected components uit te rekenen
g1 <- igraph::graph_from_edgelist(t(com1c), directed=FALSE)
comp1 <- igraph::components(g1)
gr1 <- unname(igraph::groups(comp1))

# groepeer data
l2 <- lapply(gr1, function(x){
  coln0 <- colnames(long2)[a1[x]]
  group1 <- unique(rename1[Rename %in% coln0, Group])
  if(length(group1)!=1){
    group1 <- NULL
    stop("Sommige longitudinale data lijkt toegewezen te zijn aan >1 group (in format1)")
  }
  
  coln1 <- c("ALSnr", coln0)
  old1 <- nrow(long2)
  long3 <- long2[, coln1, with = FALSE]
  long3 <- long3[rowSums(!is.na(long3))>1]
  new1 <- nrow(long3)
  meld1 <- paste0("In longitudinale ", group1, " data, ", old1-new1, "/", old1, " (", roundHJ1((old1-new1)/old1*100),"%) ",
                  "rijen verwijderd omdat ze enkel missings bevatten. Het kan hierbij een vrij hoog ",
                  "percentage zijn van rijen die verwijderd zijn. Dit komt (deels) doordat in het Progeny data ",
                  "format, de longitudinale data getrapt naast elkaar staat. ",
                  "Dit leidt automatisch to relatief veel lege cellen. Het is belangrijker om te letten op het ",
                  "percentage wat je over houdt.")
  out1 <- list(group1, meld1, long3)
  return(out1)
})
mm <- c(mm, lapply(l2, "[[", 2))

# long data
long3 <- lapply(l2, "[[", 3)
names(long3) <- unlist(lapply(l2, "[[", 1))

# cross data
coln1 <- unlist(unname(lapply(long3, function(x) colnames(x))))
coln1 <- coln1[-which(coln1=="ALSnr")]
coln2 <- colnames(d2)[!colnames(d2) %in% coln1]
d3 <- d2[, ..coln2]
d3 <- d3[rowSums(!is.na(d3))>2,] # >2 omdat ALSnr en ALSnr_NA er altijd in staan


##############################
####  WIDE TO LONG FORMAT ####
##############################

# Maak adist matrix van colnames
mat1 <- adist(colnames(d3))
colnames(mat1) <- colnames(d3)

# Maak groepen met adist =<1
list1 <- lapply(1:nrow(mat1),function(i){
  col1 <- which(mat1[i,] <=1)
  return(names(col1))
})
subgr1a <- unique(list1)

# als er geen getal in de naam van de variabele staat kan het (eigenlijk) geen longitudinale meting zijn
names1 <- unique(unlist(subgr1a))
names2 <- grep("[0-9]", names1, value = TRUE)

subgr1b <- unlist(lapply(1:length(subgr1a), function(x){
  out1 <- subgr1a[[x]] %in% names2
  out2 <- ifelse(all(out1), 1, 0)
  return(out2)
}))
subgr1 <- subgr1a[which(subgr1b==1)]

# Wat zijn de common strings in elke groep
sgstems1 <- lapply(subgr1, function(i){
  stems1 <- longest_substring_vec(unlist(i), matrix_out = TRUE)
  stems2 <- unique(c(stems1[!is.na(stems1)]))

  if(length(stems2) == 1){  # Als er meer of minder dan 1 common strings zijn, is de groepering onjuist
    return(stems2)
  } else {
    return(NA)
  }
})

names(subgr1) <- unlist(sgstems1)
sgstems2 <- unlist(unique(sgstems1))

# Check of elke groep bij een 'supergroep' hoort, op basis van terugkerende overlaps.
mat2 <- longest_substring_vec(sgstems2, matrix_out = TRUE, USE.NAMES = TRUE)


# Wat is de meest voorkomende overlap? Dit is de mogelijke supergroep van de betreffende groep
# Nieuwe manier: We wegen de mogelijkheden op (lengte van match)^2 en frequentie. 
tab1 <- apply(mat2, 1, table)
supgr1 <- sapply(tab1, function(i){
  dt1 <- as.data.table(i)
  dt1[, Weight:=(nchar(V1)^2 * N)]
  out1 <- dt1[which.max(Weight),V1]
  return(out1)
})

# Maak Hierarchy tabel:superGroup > subGroup > Var
hier <- data.table(subGroup = names(supgr1), superGroup = supgr1)
hier2 <- rbindlist(lapply(seq_along(subgr1), function(i){
  df <- data.table(Var = subgr1[[i]])
  df$subGroup <- names(subgr1[i])
  return(df)
  }))
hier3 <- hier[hier2, nomatch = 0, on = "subGroup"]

# Check if superGroup matches Group column in rename1 
rename1_w2l <- copy(rename1)[, .(Rename, Group)]
setnames(rename1_w2l, old = c("Rename"), new = c("Var"))
hier3 <- hier3[rename1_w2l, nomatch = 0, on = "Var"]
hier3[, check_group:=diag(sapply(Group, grepl, superGroup, ignore.case = TRUE))]

#check
if(any(hier3$check_group==FALSE)){
  stop("In data = hier3 komen superGroup en Group soms niet overeen. Oplossen voordat je verder gaat.")
}

# Per unieke Group, kijken welke variabelen erbij horen,
# Resulteert in een lijst van long format data.tables
wl_transform1 <- sapply(unique(hier3[check_group == TRUE, Group]), function(i){
  dt1 <- hier3[Group==i,]
  wlong1 <- d3[, c("ALSnr", dt1[, Var]), with = FALSE]
  
  # Groepeer per subgroup voor melt, deze moeten elk hun eigen kolom krijgen.
  subgroups1 <- unique(dt1[, subGroup])
  mvars <- lapply(unique(dt1[, subGroup]), grep, x = names(wlong1))
  
  # Op basis van mvars, kolommen genereren
  wlong2 <- melt(wlong1, measure.vars = mvars, 
                 variable.name = paste0("ProgenyFU_", i), value.name = subgroups1)
  #verwijder rijen met alleen missings
  wlong2[, count_na:=rowSums(is.na(wlong2))]
  wlong3 <- wlong2[count_na < length(subgroups1)]
  
  #maak indien mogelijk order obv date
  date1 <- paste0("Do", i)
  date2 <- which(colnames(wlong3)==date1)
  if(length(date2)>0){
    #nu alleen rekening gehouden met ALSnr en datum, later in script NIET ook nog rekening houd met
    # hoogte van score (indien datum mist). Omdat hoogte van score niet zo betrouwbaar is voor tijd van afname (iom Harold).
    setorderv(x = wlong3, cols = c("ALSnr", date1), order = c(1, 1), na.last = TRUE)
    
    #verwijder kolom met order (omdat het beter is om order van datum aan te houden)
    # order later pas toevoegen als je checks op datums afgerond hebt.
    #order1 <- paste0("order_", i)
    #wlong3[, c(order1):=NULL]
  }
  
  #verwijder kolommen die niet meer nodig zijn
  wlong3[, count_na:=NULL][]
  
  # Check of kolommen uit de supergroep overgeslagen worden
  dt2 <- rename1[Group==i, ]
  missedmembers <- grep(paste(dt1$Var, collapse = "|"), dt2$Rename, invert = TRUE, value = TRUE)
  meld1 <- paste0("Variabelen uit de formattabel groep ", i ," worden herkend als longitudinale data met de",
                  " subgroepen: ", paste(subgroups1, collapse = ", "))
  if (length(missedmembers) > 0){
    meld1 <- c(meld1, paste0("LET OP: Volgende variabelen horen wel bij de groep ", i, " maar worden niet",
                             " meegenomen in de longitudinale tabel: ", paste(missedmembers, collapse = ", ")))
  }
  
  out1 <- list(wlong3, meld1)
  return(out1)
}, simplify = FALSE, USE.NAMES = TRUE)

#splits data en meldingen
wlong_list1 <- lapply(wl_transform1, "[[", 1)
wlong_mm1 <- lapply(wl_transform1, "[[", 2)
names(wlong_mm1) <- NULL
mm <- c(mm, wlong_mm1)

#voeg wlong_list1 samen met long data die al in long3 zit
long3 <- c(long3, wlong_list1)

#verwijder wide2long columns uit d3
d3 <- d3[, !colnames(d3) %in% unique(unlist(subgr1)), with = FALSE]


##############################
####  SUMSCORES ALSFRS-R  ####
##############################

# maak vraag 5 van de ALSFRS-R in orde
DEFINITION_q5(A = "ALSFRS_q5a", B = "ALSFRS_q5b", data = long3$ALSFRS_R)

# maak sumsores
long3$ALSFRS_R[, ALSFRS_score:=rowSums(long3$ALSFRS_R[, c(DEFINITION_Q_ALSFRSR())])]

# check nog of 0 <= ALSFRS_score <= 48 is
# (ip niet nodig omdat dit al door format1.xlsx gedekt wordt, maar voor de zekerheid ingebouwd)
old1 <- countNA(long3$ALSFRS_R, cols = "all")
set(long3$ALSFRS_R, i = which(long3$ALSFRS_R$ALSFRS_score>48), j = DEFINITION_Q_ALSFRSR(), value = NA)
set(long3$ALSFRS_R, i = which(long3$ALSFRS_R$ALSFRS_score<0), j = DEFINITION_Q_ALSFRSR(), value = NA)
new1 <- countNA(long3$ALSFRS_R, cols = "all")
mm <- c(mm, list(summaryNA(old = old1, new = new1, name_data = "long3$ALSFRS_R")))

# neem alleen unieke rijen (date zit er ook in) en orden ALSFRS-R
# hoog is vroeg (-1 dus)
long3$ALSFRS_R <- unique(long3$ALSFRS_R)
setorderv(x = long3$ALSFRS_R, cols = c("ALSnr", "DoALSFRS", "ALSFRS_score"), order = c(1, 1, -1), na.last = TRUE)
mm <- c(mm, paste0("MELDING: Longitudinale data van ALSFRS geordend: ",
                   "Eerste ALSFRS (qua datum) of anders hoogste ALSFRS (qua score) bovenaan. ",
                   "Omdat er mogelijk later nog datums op NA gezet worden nog geen kolom met order toegevoegd."))


#######################
####  CHECK DATES  ####
#######################

# make dependencies uniform
dep2a <- dep2[value==1]
setcolorder(dep2a, c("to", "from", setdiff(names(dep2a), c("from", "to"))))
setnames(dep2a, old = c("to", "from"), new = c("from", "to"))
dep2a[, value:=-1]
dep3 <- rbind(dep2[value!=1], dep2a)[, uitleg:="'from' komt eerder dan 'to'."][]
if(all(dep3$value==-1)==FALSE){
  stop("In dep3 zijn niet alle values -1. Oplossen voordat je verder gaat.")
}

# merge cross & long data (tijdelijk)
key1 <- lapply(long3, function(x){
  setkey(x, ALSnr)
})
long4 <- unique(Reduce(merge_list_cart, key1)) # hier unique toegevoegd, zou volgens mij moeten kunnen @Harold wat vind jij?
setkey(d3, ALSnr)
merge1 <- merge(d3, long4, all.x = TRUE)

# zorg dat alle namen in dep3 ook voorkomen in colnames(merge1)
names_dep3a <- unique(unlist(dep3[, .(from, to)]))
names_dep3b <- names_dep3a %in% colnames(merge1)

# maak melding indien niet alle namen in dep3 voorkomen in colnames(merge1)
if(any(names_dep3b==FALSE)){
  mm <- c(mm, paste0("LET OP! ", "De volgende naam/namen (in dep3) komt/komen niet voor in de dataset (merge1): ",
         names_dep3a[!names_dep3b], ". Dit kan er op wijzen dat de namen van de variabelen ",
         "en dependencies (Sheet2 van Format_v1.xlsx) niet uniform zijn."))
}

# filter de juiste rijen uit dep3
dep4 <- dep3[from %in% names_dep3a[names_dep3b]][to %in% names_dep3a[names_dep3b]]

#maak graph obv dependencies (wat beinvloed wat)
g2 <- igraph::graph_from_edgelist(as.matrix(dep4[, .(from, to)]), directed = TRUE)
g2_closeness <- igraph::closeness(g2)

# orden dep4 op closeness
# misschien voegt dit niet veel toe maar het voelt logisch om eerst de variabelen
# te checken die het verste "upstream" liggen (in het algemeen betekend dit dat je 
# DoB als eerste zult onderzoeken en DoDeath als laatste, en nog verschillende daar tussenin uiteraard).
cl1 <- data.table(g2_closeness, names(g2_closeness))
colnames(cl1) <- c("closeness", "from")
dep4 <- dep4[cl1, nomatch = 0, on = "from"]; setnames(dep4, old = "closeness", new = "closeness_from")
colnames(cl1) <- c("closeness", "to")
dep4 <- dep4[cl1, nomatch = 0, on = "to"]; setnames(dep4, old = "closeness", new = "closeness_to")
setorder(dep4, -closeness_from, -closeness_to)

# zet datums op NA indien ze niet voldoen aan het vooraf gedefinieerde sequentiele patroon.
# indien er een fout gevonden wordt, zet dan ook alle variabelen "downstream" (i.e. tussen
# onderzochte variabele en (in tijd) de laatste variabele, i.e. meestal dood) op NA.
old1 <- countNA(merge1, cols = "all")
out1 <- list()
for (x in 1:nrow(dep4)){
  g2_spath <- igraph::all_shortest_paths(g2, from = dep4$from[x])
  NA1 <- unique(names(unlist(g2_spath$res))) # "downstream" variables
  
  # zet alles op NA wat "downstream" connected is
  # dit is zeer streng maar waarschijnlijk wel het veiligste
  #set(merge1, i = merge1[get(dep4$from[x]) > get(dep4$to[x]), which = TRUE],
  #    j = NA1, value = NA)
  
  #Bovenstaande was de oude methode. Hier mijn voorstel (HT)
  out1[[x]] <- merge1[get(dep4$from[x]) > get(dep4$to[x]), j=c("ALSnr",NA1), with=F]
  
  
  
  # zet zowel "from" als "to" (uit dep4) op NA indien datum niet in de juist opeenvolging staan
  # dit is het strengste wat je kunt doen, maar daardoor voor nu waarschijnlijk wel goed.
  #set(merge1, i = merge1[get(dep4$from[x]) > get(dep4$to[x]), which = TRUE],
  #    j = c(dep4$from[x], dep4$to[x]), value = NA)
}
date_errors <- unique(rbindlist(out1,fill=TRUE))

meld_dates1 <- list()
any_dates1 <- list() 
for (x in colnames(date_errors)[colnames(date_errors)!="ALSnr"]) {
  dt1 <- subset(date_errors, subset = TRUE, select = c("ALSnr", x))
  dt2 <- na.omit(dt1, all.vars = TRUE)
  
  if(nrow(dt2)==0){
    meld_dates1[[x]] <- paste0("No date irregularities for ", x)
    any_dates1[[x]] <- FALSE
  } else {
    any_dates1[[x]] <- TRUE
    
    if (any(grepl(x, colnames(d3)))) {
      set(d3, i = d3[dt2, on = colnames(dt2), which = TRUE], j = x, value = NA)
      meld_dates1[[x]] <- paste0("Due to irregularities in dates, ",nrow(dt2) ," values for ", x," in object d3 have been set to NA" )
    } else {
      collist_long3 <- sapply(long3,colnames)
      set(long3[[grep(x,collist_long3)]], i= long3[[grep(x,collist_long3)]][dt2, on = colnames(dt2), which = TRUE], j = x, value = NA)
      meld_dates1[[x]] <- paste0("Due to irregularities in dates, ",nrow(dt2) ," values for ", x," in object long3$",
                                 names(collist_long3)[grep(x,collist_long3)], " have been set to NA" )
    }
  }
}


if (any(unlist(any_dates1))){
  meld_dates1$Note <- paste0("For an overview of all irregular dates, print(date_errors)")
}
names(meld_dates1) <- NULL
mm <- c(mm, list(unlist(meld_dates1)))

new1 <- countNA(merge1, cols = "all")
reason1 <- "deze datum voor of na een andere datum voorkwam (wat onmogelijk is), zoals bijv. DoO voor DoB."
mm <- c(mm, list(summaryNA(old1, new1, name_data = "merge1", reason = reason1)))


###############################################
####  DEFINITE SPLIT OF CROSS & LONG DATA  ####
###############################################

# colnames in long3
coln_long3 <- lapply(long3, function(x){
  colnames(x)
})

# longitudinale dataset
long4 <- lapply(coln_long3, function(x){
  df1 <- merge1[, ..x]
  df1[, count_na:=rowSums(is.na(df1))]
  df2 <- df1[count_na < (ncol(df1)-2)] # -2 omdat je altijd ALSnr en nu ook count_na hebt
  df3 <- unique(df2) # hier ook weer unique gedaan omdat je volgens mij geen longitudinale waardes kunt hebben die identiek zijn @Harold: wat vind jij?
  df3[, count_na:=NULL]
  return(df3)
})

# cross-sectionele dataset
coln_cross <- c("ALSnr", setdiff(colnames(merge1), unique(unlist(coln_long3))))
d4 <- unique(merge1[, ..coln_cross])


# ER GAAT HIER ERGENS IETS FOUT
# zie hieronder
# OK. Fout zit in vergelijk met longitudinale data. Onderstaande subjecten hebben een
# ALSFRS-R gehad voor de diagnose. Dit nog verder toegevoegd aan issue #4
coln_test <- unique(c(colnames(d4), unlist(dep4[, .(from, to)])))
d4[ALSnr %in% d4$ALSnr[duplicated(d4$ALSnr)]] #hier fout
merge1[ALSnr %in% d4$ALSnr[duplicated(d4$ALSnr)], coln_test, with = FALSE] #hier fout
d3[ALSnr %in% d4$ALSnr[duplicated(d4$ALSnr)], colnames(d4), with = FALSE] #hier nog niet





# orden longtidunale data (voeg order toe). Dit komt in nieuw script (voor databewerking).

