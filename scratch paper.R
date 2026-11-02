

















test_result <- t.test(ES_ANX ~ CVPK, data = CHILD_SCORED)

test_result

anxiety_result <- anxiety_result <- misty::test.welch(
  ES_ANX ~ CVPK,
  data = CHILD_SCORED,
  descript = TRUE
)

print(anxiety_result$result)

anxiety_result$result$pval


names(anxiety_result$result)


r <- anxiety_result$result

# con <- which(r$group == 0)
# mal <- which(r$group == 1)


con <- r$group == 0
mal <- r$group == 1


x <- CHILD_SCORED$ES_ANX[
  CHILD_SCORED$CVPK %in% c(0, 1) &
    !is.na(CHILD_SCORED$ES_ANX) ]

ANXIETY_BLOCK <- data.frame(
  ANALYSIS = "02",
  VARNAME  = "ES_ANX",
  
  `_STAT_` = c("MEAN", "STD", "N",
               "TTEST WELCH", "PVALUE"),
  
  MAL = c(r$m[mal], r$sd[mal], r$n[mal],
          NA_real_, NA_real_),
  
  CON = c(r$m[con], r$sd[con], r$n[con],
          NA_real_, NA_real_),
  
  ALL = c(mean(x), sd(x), length(x),
          r$t[!is.na(r$t)],
          r$pval[!is.na(r$pval)]),
  
  check.names = FALSE
)


print(ANXIETY_BLOCK)


ES_VARIABLES <-    names(CHILD_SCORED)[startsWith(names(CHILD_SCORED), "ES")]

analysis_vars <- names(CHILD_SCORED)[
  match("ES_ANX", names(CHILD_SCORED)):ncol(CHILD_SCORED)]




for (variable in analysis_vars) {
  print(variable)
}

for (variable in analysis_vars) {
  
  test_formula <- reformulate("CVPK", response = variable)
  
  test_result <- misty::test.welch(
    test_formula,
    data = CHILD_SCORED,
    descript = TRUE
  )
}


ES_VARIABLES <-    names(CHILD_SCORED)[startsWith(names(CHILD_SCORED), "ES")]



#BEGIN FUNCTION DEFINITION




hipic_mean_diffs <- function(DATA) {



options(scipen = 999)
cat("\014")

analysis_vars <- names(DATA)[
  match("ES_ANX", names(DATA)):ncol(DATA)]

runs <- c(0,1,2)



for (variable in analysis_vars) {
  print(variable)
}

BASE <- data.frame()



for ( sex in runs  ) {


for (variable in analysis_vars) {
  
  
  test_formula <- reformulate("CVPK", response = variable)
  
  test_result <- misty::test.welch(
    test_formula,
    # data = DATA[DATA$MVF == sex, ],
    data = DATA[ if (sex < 2) DATA$MVF == sex else TRUE, ],
    descript = TRUE
  )
  
  r<- test_result$result
  con <- r$group == 0
  mal <- r$group == 1
  
  print(variable)
  print(r$t)
  print(r$pval)
  
  
  x <- DATA[[variable]][
        DATA$CVPK %in% c(0, 1) &
        !is.na(DATA[[variable]]) &
                        (if (sex < 2) DATA$MVF == sex else TRUE)
  ]
  
  
#   if(sex <2) {
# 
# 
#      x <- DATA[[variable]][DATA$CVPK %in% c(0,1) &
#                                   DATA$MVF == sex &
#                                     !is.na(DATA[[variable]])]  }
# 
# else {
# 
#       x <- DATA[[variable]][DATA$CVPK %in% c(0,1) &
#                 !is.na(DATA[[variable]])]   }
  
  
#  x <- DATA$ES_ANX[ DATA$CVPK %in% c(0, 1) &  !is.na(DATA$ES_ANX) ]
 
  
   
  temp <- data.frame(
    MODEL = sprintf("%02d", sex   ),
    VARNAME  = variable,
    
    `_STAT_` = c("MEAN", "STD", "N",
                 "TTEST WELCH", "PVALUE"),
    
    MAL = c(r$m[mal], r$sd[mal], r$n[mal],
            NA_real_, NA_real_),
    
    CON = c(r$m[con], r$sd[con], r$n[con],
            NA_real_, NA_real_),
    
    ALL = c(mean(x), sd(x), length(x),
            r$t[!is.na(r$t)],
            r$pval[!is.na(r$pval)]),
    
    check.names = FALSE  )
  
  BASE <- rbind(BASE, temp)
  
  
}

}
    return(BASE)
}



BASEC <- hipic_mean_diffs(CHILD_SCORED)
BASEP <- hipic_mean_diffs(PARENT_SCORED)



library(openxlsx)

file_path <- "/Users/Kevin/OneDrive/Documents/Excel/HiPIC Exploratory.xlsx"

wb <- if (file.exists(file_path)) {
  openxlsx::loadWorkbook(file_path)
} else {
  openxlsx::createWorkbook()
}

if (!"BASEC" %in% names(wb)) {
  openxlsx::addWorksheet(wb, "BASEC")
}
openxlsx::writeData(wb, "BASEC", BASEC)
openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)


wb <- if (file.exists(file_path)) {
  openxlsx::loadWorkbook(file_path)
} else {
  openxlsx::createWorkbook()
}

if (!"BASEP" %in% names(wb)) {
  openxlsx::addWorksheet(wb, "BASEP")
}
openxlsx::writeData(wb, "BASEP", BASEP)
openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)




write.csv(PARENT_SCORED,
          "QC",
          row.names = FALSE)



write.csv(
  HIPIC_CROSSWALK,
  "HIPIC_CROSSWALK.csv",
  row.names = FALSE
)









head(BASE)



ANXIETY_BLOCK <- data.frame(
  ANALYSIS = "02",
  VARNAME  = "ES_ANX",
  
  `_STAT_` = c("MEAN", "STD", "N",
               "TTEST WELCH", "PVALUE"),
  
  MAL = c(r$m[mal], r$sd[mal], r$n[mal],
          NA_real_, NA_real_),
  
  CON = c(r$m[con], r$sd[con], r$n[con],
          NA_real_, NA_real_),
  
  ALL = c(mean(x), sd(x), length(x),
          r$t[!is.na(r$t)],
          r$pval[!is.na(r$pval)]),
  
  check.names = FALSE
)


############################


library(psych)

parent_items <- PARENT_REVERSED[
  , grepl("^P[0-9]+_", names(PARENT_REVERSED))
]

set.seed(20261101)

parent_parallel <- psych::fa.parallel(
  parent_items,
  fa = "pc",
  cor = "cor",
  n.iter = 500,
  quant = .95,
  sim = FALSE
)

parent_pc5 <- psych::principal(
  parent_items,
  nfactors = 5,
  rotate = "varimax",
  scores = FALSE
)

print(parent_pc5, cut = .40, sort = TRUE)


L <- as.matrix(parent_pc5$loadings)
rownames(L) <- sub("_.*", "", rownames(L))

print(round(L, 3), max = 2000)
parent_pc5$Vaccounted




parent_pc5_promax <- psych::principal(
  parent_items,
  nfactors = 5,
  rotate = "promax",
  scores = FALSE
)

print(parent_pc5_promax, cut = .40, sort = TRUE)

parent_pc5_promax$Philength(colnames(PARENT_REVERSED))



colnames(PARENT_REVERSED)



#DEFINE FUNCTION



hipic_quest_distrib <- function(DATA) {
  
  

lastcol <- length(colnames(DATA))
lastcol

# 1. Store the question names in their original column order

question_names <- names(DATA) %>%
  .[grepl("^[PC][0-9]+_", .)]

#2. Store questions that were reverse scored

revscored <- c(
  27, 63, 99, 87, 42, 60, 1, 37, 5, 41, 50, 122, 16, 124,
  20, 110, 120, 138, 79, 103, 121, 139, 17, 35, 53, 89, 107,
  143, 8, 44, 62, 80, 129,49, 55, 84 )



#3. find quesiton prefix value

prefix <- substr( grep("^[PC]1_", colnames(DATA), value = TRUE), 1, 1)




#as.integer(sub("^P([0-9]+)_.*$", "\\1", "P5_IMAGINE_haslittleimagination"))



x<-
  DATA %>% 
pivot_longer( cols=  matches( paste0("^", prefix, "[0-9]") ),
              names_to = "QUESTIONS",
              values_to = "RESPONSE") %>% 
  
  mutate(  KEYNUM = sprintf("Q%03d",  as.integer(sub(     paste0("^", prefix, "([0-9]+)_.*$"), "\\1",  QUESTIONS))) ,
           RS = case_when( as.integer(sub(      paste0("^", prefix, "([0-9]+)_.*$"), "\\1", QUESTIONS)) %in% revscored ~ 'Y', TRUE ~ "") ,
    
    
           # KEYNUM = sprintf("Q%03d",  as.integer(sub("^P([0-9]+)_.*$", "\\1",  QUESTIONS))) ,
           # RS = case_when( as.integer(sub("^P([0-9]+)_.*$", "\\1", QUESTIONS)) %in% revscored ~ 'Y', TRUE ~ "") ,
           
          POSITION = sprintf("Q%03d", match(QUESTIONS, question_names)) 
          
                ) %>% 
  
              select( KEYNUM, POSITION, RS, QUESTIONS, RESPONSE) %>% 
              group_by( KEYNUM, POSITION, RS, QUESTIONS, RESPONSE) %>% 
              count(name = "TALLY", sort = TRUE) %>% 
              group_by(KEYNUM, POSITION, RS, QUESTIONS) %>% 
  
              mutate( QTALLY = sum(TALLY),
                      PCT = TALLY/QTALLY) %>% ungroup %>%  arrange( KEYNUM,  desc(PCT) ) %>%  select( KEYNUM, POSITION, RS, QUESTIONS, RESPONSE, PCT) %>% 


  

            print(., n=30)

x %>%
  dplyr::filter(RS == "Y") %>%
  distinct(KEYNUM)



x %>%  select(KEYNUM, POSITION, RS, QUESTIONS, RESPONSE, PCT) %>%  dplyr::filter( PCT >=0.60 ) %>% arrange( desc(PCT)) %>% print(., n=50)


#  return(x)



}


hipic_quest_distrib(PARENT_REVERSED)
hipic_quest_distrib(CHILD_REVERSED)


