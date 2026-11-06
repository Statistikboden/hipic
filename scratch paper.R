










.small-note[**`r sum(MAP$Match != "None")` of the 144 HiPIC items have a BFI-2 counterpart:** `r N["Equivalent"]` Equivalent, `r N["Similar"]` Similar, `r N["Related"]` Related; `r N["None"]` have none. Search, sort or filter the table below.]







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
  PARENT_REVERSED,
  "PARENT_REVERSED",
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






library(rstatix)


#Goodness of fit - Chi-Square
PARENT_RAW %>% select(CVPK) %>% table() %>%  rstatix::chisq_test(., p= c(0.5, 0.5))
CHILD_RAW %>% select(CVPK) %>% table() %>%  rstatix::chisq_test(., p= c(0.5, 0.5))


#Missingness

PMISS <- PARENT_RAW %>%
  select(ID4, CVPK, last_col(offset=143) : last_col()) %>%
  rowwise() %>% 
  mutate(MISSING = sum(is.na(c_across(last_col(offset = 143):last_col())))) %>%
  ungroup() %>%
  filter(MISSING >= 3) %>%
  select(ID4, CVPK, MISSING)

nrow(PMISS)

CMISS <- CHILD_RAW %>%
  select(ID4, CVPK, last_col(offset=143) : last_col()) %>%
  rowwise() %>% 
  mutate(MISSING = sum(is.na(c_across(last_col(offset = 143):last_col())))) %>%
  ungroup() %>%
  filter(MISSING >= 3) %>%
  select(ID4, CVPK, MISSING)

nrow(CMISS)
    
PARENT_RAW %>% anti_join( PMISS[, c("ID4")], by="ID4")  %>%  select(CVPK) %>% table() %>%  rstatix::chisq_test(., p= c(0.5, 0.5))
CHILD_RAW  %>% anti_join( CMISS[, c("ID4")], by="ID4")  %>%  select(CVPK) %>% table() %>%  rstatix::chisq_test(., p= c(0.5, 0.5))



# Mean Nutritional Groups

PARENT_RAW %>% filter(!ID4 %in% pull(PMISS, ID4))  %>% 
  misty::test.welch( AGEEXAM ~ CVPK, data = .,  descript=T)

CHILD_RAW %>% filter(!ID4 %in% pull(CMISS, ID4))  %>% 
  misty::test.welch( AGEEXAM ~ CVPK, data = .,  descript=T)


# Percent Male


PARENT_RAW %>% filter(!ID4 %in% pull(PMISS, ID4))  %$% 
  sjPlot::plot_xtab(CVPK, MVF, margin = "row",
            bar.pos = "stack", coord.flip = F,
            title = "Sex Distribution by Nutritional Group",
            axis.titles = c("CVPK"),
            legend.title = "Sex",
            geom.colors = c("#006cc5", "#cbcccb"),
                  show.summary = TRUE)

      PARENT_RAW %>% filter(!ID4 %in% pull(PMISS, ID4)) 
      
      PARENT_RAW %>% filter(!ID4 %in% pull(PMISS, ID4))  %$% 
        chisq.test(CVPK, MVF, correct = TRUE) %>% .$observed %T>% print() %>% prop.table(margin = 1) %>% round(3) *100
      
      


CHILD_RAW %>% filter(!ID4 %in% pull(CMISS, ID4))  %$% 
  sjPlot::plot_xtab(CVPK, MVF, margin = "row",
                    bar.pos = "stack", coord.flip = F,
                    title = "Sex Distribution by Nutritional Group",
                    axis.titles = c("CVPK"),
                    legend.title = "Sex",
                    geom.colors = c("#006cc5", "#cbcccb"),
                    show.summary = TRUE)

    CHILD_RAW %>% filter(!ID4 %in% pull(CMISS, ID4))  %$% 
      chisq.test(CVPK, MVF, correct = TRUE) %>% .$observed %T>% print() %>% prop.table(margin = 1) %>% round(3) *100
    
    
    
test_function <-   
    
  
function(DATA) {
  
  
  temp <- deparse(substitute(DATA));
  DSN <- get(paste0(temp, "_RAW"))  

#  MDSN <-  get(paste0(str_sub(temp,1,1), "MISS"))
  
  #Goodness of Fit
  
  cat("\n====", "Goodness of Fit (Before)", "====\n")
  DSN %>% select(CVPK) %>% table() %>%  rstatix::chisq_test(., p= c(0.5, 0.5)) %>% print(.)
  
  
  #Missingness
  
  cat("\n====", "Goodness of Fit (After)", "====\n")
  MDSN <- DSN %>%
    select(ID4, CVPK, last_col(offset=143) : last_col()) %>%
    rowwise() %>% 
    mutate(MISSING = sum(is.na(c_across(last_col(offset = 143):last_col())))) %>%
    ungroup() %>%
    filter(MISSING >= 3) %>%
    select(ID4, CVPK, MISSING)
  
  
  cat("Excluded for >= 3 missing items:", nrow(MDSN), "\n")
  DSN %>% anti_join( MDSN[, c("ID4")], by="ID4")  %>%  select(CVPK) %>% table() %>%  
          rstatix::chisq_test(., p= c(0.5, 0.5)) %>% print(.)
    
  
  # Mean Nutritional Groups
  
  cat("\n====", "Mean Age by Nutritional Group", "====\n")
  DSN %>% filter(!ID4 %in% pull(MDSN, ID4))  %>% 
    misty::test.welch( AGEEXAM ~ CVPK, data = .,  descript=T)  
    
  
  
  
  # Percent Male
  
  
  cat("\n====", "Percentage of Male Subjects by Nutritional Group", "====\n")
  DSN %>% filter(!ID4 %in% pull(MDSN, ID4))  %$% 
    sjPlot::plot_xtab(CVPK, MVF, margin = "row",
                      bar.pos = "stack", coord.flip = F,
                      title = "Sex Distribution by Nutritional Group",
                      axis.titles = c("CVPK"),
                      legend.title = "Sex",
                      geom.colors = c("#006cc5", "#cbcccb"),
                      show.summary = TRUE) %>% print(.)
  
  
  DSN %>% filter(!ID4 %in% pull(MDSN, ID4))  %$% 
    chisq.test(CVPK, MVF, correct = TRUE) %>% .$observed %T>% print() %>% prop.table(margin = 1) %>% round(3) *100 
 
   DSN %$% chisq.test(CVPK, MVF, correct = TRUE) %>% print()
  
  
}
    
   test_function(CHILD) 
    
   
   
   
   
    
   LABEL    <- deparse(substitute(DSN))          # "PARENT_RAW"
   BASE     <- sub("_RAW$", "", LABEL)           # "PARENT"
   REVERSED <- get(paste0(BASE, "_REVERSED"))    # fetches PARENT_REVERSED
    
    #Goodness of fit - Chi-Square
    PARENT_RAW %>% select(CVPK) %>% table() %>%  rstatix::chisq_test(., p= c(0.5, 0.5))
    CHILD_RAW %>% select(CVPK) %>% table() %>%  rstatix::chisq_test(., p= c(0.5, 0.5))
    
    
    CHILD_RAW %$% table(CVPK)                                   # expect 35 (CON), 44 (MAL)
    fisher.test(matrix(c(2, 42, 3, 32), nrow = 2))              # missingness p, expect ~.65
    
    
    
    
    
    
    
    
    
    names(HIPIC_CROSSWALK)      # what columns we have to choose from
    head(HIPIC_CROSSWALK, 15)   # the first 15 rows of the concordance
    
    head(PARENT_ALIGNED, 15)    # the realignment itself: Q_ORIG (parent position) -> Q_NEW (child position)
    
    
    
    FACETS <- list(
      Anxiety = ES_ANX, `Self-Confidence` = ES_CON,
      Energy = EX_ENE, Expressiveness = EX_EXP, Optimism = EX_OPT, Shyness = EX_SHY,
      Creativity = IM_CRE, Intellect = IM_INT, Curiosity = IM_CUR,
      Altruism = BE_ALT, Dominance = BE_DOM, Egocentrism = BE_EGO,
      Compliance = BE_COM, Irritability = BE_IRR,
      Concentration = CO_CNC, Perseverance = CO_PER, Order = CO_ORD,
      `Achievement Motivation` = CO_ACH)
    
    FACET_KEY <- stack(FACETS)   # one row per item: values = item #, ind = facet name
    nrow(FACET_KEY)              # check: should be 144
    
    
    
    XWALK_SLIDE <- PARENT_ALIGNED %>%
      arrange(Q_ORIG) %>%
      slice(1:16) %>%
      left_join(FACET_KEY, by = c("Q_NEW" = "values")) %>%
      transmute(`Parent #`       = Q_ORIG,
                `Parent Wording` = PARENT_Q_WORDING,
                `Child #`        = Q_NEW,
                `SAS Varname`       = BNS_NAME_C,
                Facet            = as.character(ind))
    
    XWALK_SLIDE                               # look before saving
    saveRDS(XWALK_SLIDE, "tab_crosswalk.rds")
    
    
    
    
    
    
    
    
    
########################
    


    
# Step 1: Evaluate each dataset’s suitability for PCA/FA
# 
# 1a. The correlation matrix: its rank and eigenvalues. This shows what 74 children (and 77 parents) can support at item level.
# 1b. KMO and Bartlett’s test: at item level, expected to fail or warn, plus facet level.  
    
    
    C_ITEMS <- CHILD_REVERSED  %>% select(last_col(offset = 143):last_col())
    P_ITEMS <- PARENT_REVERSED %>% select(last_col(offset = 143):last_col())
    
    nrow(C_ITEMS)
    nrow(P_ITEMS)
    
    
    #Children's dataset
    
    R_C <- cor(C_ITEMS)       # 144 x 144 correlation matrix
    qr(R_C)$rank              # how many independent dimensions it actually holds
    EV_C <- eigen(R_C)$values # the 144 eigenvalues, largest first
    sum(EV_C > 1e-10)         # how many are meaningfully above zero
    
    
    #Parent's dataset
    
    R_P <- cor(P_ITEMS)       # 144 x 144 correlation matrix
    qr(R_P)$rank              # how many independent dimensions it actually holds
    EV_P <- eigen(R_P)$values # the 144 eigenvalues, largest first
    sum(EV_P > 1e-10)         # how many are meaningfully above zero
    
    
    # Analysis:
    #   
    # the data can describe at most 73 (children) and 76 (parents) independent dimensions of the 144 items; the rest are mathematically empty. 
    # That isn’t a flaw in the data collection; it’s what happens whenever there are more items than people. 
    # That’s why the item-level FA has to be presented as exploratory, and why the matrix will need smoothing.
    # This is also what your scree plot will show: the eigenvalues fall to zero after the 73rd (or 76th).
    
    
    
#Step 2. KMO Test
    

    psych::KMO(R_C)
    psych::cortest.bartlett(R_C, n = 74)


    
    psych::KMO(R_P)
    psych::cortest.bartlett(R_P, n = 77)
    
    
    
    
    # Finding 1: The item correlation matrices are singular. The analysis includes 144 items for 74 children and 77 parents. 
    # With fewer respondents than items, a correlation matrix can hold at most n − 1 independent dimensions. 
    # The child matrix has rank 73 (73 non-zero eigenvalues, 71 zero) and the parent matrix has rank 76 (76 non-zero, 68 zero). 
    # This is a mathematical consequence of the sample-to-item ratio, not a flaw in the data. 
    # Any item-level factor solution therefore rests on a smoothed correlation matrix and should be treated as exploratory.
    # 
    # Finding 2: Standard factorability tests cannot be computed at item level. 
    # Because the matrices are singular, the Kaiser-Meyer-Olkin measure of sampling adequacy (KMO) could not be calculated for either sample: 
    # the matrices could not be inverted (reciprocal condition numbers about 10⁻²⁰), and the 0.50 values psych reports are fallback placeholders, not real estimates. 
    # Bartlett’s test of sphericity was likewise undefined (χ² = ∞, df = 10,296), because the determinant of each matrix is zero. 
    # Factorability will instead be assessed at facet level (18 facets), where the matrices are of full rank.
    # 
    # One-line version, for a slide or an email:
    # At item level (144 items; n = 74 children, 77 parents), the correlation matrices are singular (rank 73 and 76), 
    # so KMO and Bartlett’s test cannot be computed. Item-level results are exploratory, and factorability will be checked at facet level.
    # 
    # Item-level FA is feasible but underpowered at this sample size; results are exploratory.”
    
    
   options( scipen=0)
    
    # Repeat this for the Facets, no domains
    
    
    C_FACETS <- CHILD_SCORED  %>% select(matches("^(ES|EX|IM|BE|CO)_"))
    P_FACETS <- PARENT_SCORED %>% select(matches("^(ES|EX|IM|BE|CO)_"))
    
    dim(C_FACETS)   
    dim(P_FACETS)  
    
    
    psych::KMO(C_FACETS)
    psych::cortest.bartlett(C_FACETS)
    
    psych::KMO(P_FACETS)
    psych::cortest.bartlett(P_FACETS)
    
    
    # At facet level (18 facets), both samples were suitable for factoring: KMO = .85 (children) and .80 (parents); 
    # Bartlett’s test p < .001 in both. Individual facet MSAs were .70 or higher apart from child ES_ANX (.49) and 
    # parent BE_EGO (.57), and several parent facets in the .66–.69 range; no facets were removed.
    
    
    
    
  #3.  Using the psych package, determine the default recommended number of factors.
    
    #Children - QUESTIONS 144
    psych::fa.parallel(C_ITEMS, fm = "minres", fa = "fa")
    psych::scree(R_C, pc = FALSE)
    
    tibble(COMPONENT  = seq_along(EV_C),
           EIGENVALUE = round(EV_C, 2),
           PCT_VAR    = round(100 * EV_C / sum(EV_C), 1),
           CUM_PCT    = round(100 * cumsum(EV_C) / sum(EV_C), 1)) %>%
      head(10)
    
    
    # Parents - QUESTIONS 144
    psych::fa.parallel(P_ITEMS, fm = "minres", fa = "fa")
    psych::scree(R_P, pc = FALSE)
    
    tibble(COMPONENT  = seq_along(EV_P),
           EIGENVALUE = round(EV_P, 2),
           PCT_VAR    = round(100 * EV_P / sum(EV_P), 1),
           CUM_PCT    = round(100 * cumsum(EV_P) / sum(EV_P), 1)) %>%
      head(10)
    
    
    # Parents, item level: parallel analysis (minres, 144 items, n = 77) suggested 6 factors. 
    # The first two principal-component eigenvalues were 28.8 (20.0% of variance) and 16.8 (11.7%); 
    # the first five components explained 43.0% and the first six 46.2%. 
    # The same smoothing warnings applied as for the children. In both samples, two dominant dimensions preceded a gradual decline, 
    # and parallel analysis suggested 6 factors, against a theoretical expectation of 5.
    
    
    
    #Repeat on  Facets
    
    
    set.seed(2026)
    psych::fa.parallel(C_FACETS, fm = "minres", fa = "fa")
  
    
    psych::fa.parallel(P_FACETS, fm = "minres", fa = "fa")
    
    
    # Facet level (18 facets): parallel analysis (minres) suggested 2 factors in both samples (children n = 74, parents n = 77), 
    # with no estimation warnings. Together with the two dominant item-level eigenvalues, this suggests two broad dimensions are 
    # the strongest structure detectable at this sample size. 
    # The five-factor solution will still be estimated as the theory-driven model for the HiPIC/NEO comparison, 
    # with the two-factor solution examined for context.
    
    
    
    # Begin Exploratory Factor Analysis
    
    
    
    #CHILDREN
    
    FA5_CNOROT <- psych::fa(C_ITEMS, nfactors = 5, fm = "minres", rotate = "none")
    
    FA5_CNOROT$Vaccounted                                    # variance explained by each factor
    print(FA5_CNOROT$loadings, cutoff = 0.30, sort = TRUE)   # loadings, readable version
    
    
    L <- unclass(FA5_CNOROT$loadings)
    
    tibble(ITEM     = rownames(L),
           MR2      = L[, "MR2"],
           REVERSED = as.integer(sub("^C(\\d+)_.*", "\\1", rownames(L))) %in% REVERSE_ITEMS) %>%
      filter(abs(MR2) >= .30) %>%
      count(REVERSED, DIRECTION = ifelse(MR2 > 0, "positive", "negative"))
    
    # In the unrotated five-factor solution for children, the second factor (9.0% of variance) showed a perfect split 
    # by scoring direction: all 34 non-reversed items loading ≥ .30 loaded positively, and all 21 reverse-scored items loaded negatively. 
    # In the original response metric, all 55 items load in the same direction, consistent with an acquiescence (agreement) 
    # response style rather than a substantive personality dimension.
    # 
    
    
    FA5_CVMAX <- psych::fa(C_ITEMS, nfactors = 5, fm = "minres", rotate = "varimax")
    
    FA5_CVMAX$Vaccounted                                    # variance explained by each factor
    print(FA5_CVMAX$loadings, cutoff = 0.30, sort = TRUE)   # loadings, readable version
    
    
    
    
    #PARENTS
    
    FA5_PNOROT <- psych::fa(P_ITEMS, nfactors = 5, fm = "minres", rotate = "none")
    
    FA5_PNOROT$Vaccounted                                    # variance explained by each factor
    print(FA5_PNOROT$loadings, cutoff = 0.30, sort = TRUE)   # loadings, readable version
    
    
    L <- unclass(FA5_PNOROT$loadings)
    
    tibble(ITEM     = rownames(L),
           MR2      = L[, "MR2"],
           REVERSED = as.integer(sub("^P(\\d+)_.*", "\\1", rownames(L))) %in% REVERSE_ITEMS) %>%
      filter(abs(MR2) >= .30) %>%
      count(REVERSED, DIRECTION = ifelse(MR2 > 0, "positive", "negative"))
    
    
    
    
    # In the unrotated five-factor solution for children, the second factor (9.0% of variance) showed a perfect split 
    # by scoring direction: all 34 non-reversed items loading ≥ .30 loaded positively, and all 21 reverse-scored items loaded negatively. 
    # In the original response metric, all 55 items load in the same direction, consistent with an acquiescence (agreement) 
    # response style rather than a substantive personality dimension.
    # 
    
    
    
    
    
    
    # It isn’t intuition. It’s a few specific questions asked of each factor, in order. Here’s the checklist I was running.
    # 
    # 1. What do the items on this factor have in common: content, or form?
    #   A real trait factor groups items by topic. An anxiety factor collects worry, fear and panic items. 
    # So for each factor I ask whether the top items share a topic.
    # 
    # On MR1 they don’t. “Defends the weak” (benevolence), “perseveres” (conscientiousness), “quick to understand” (imagination), 
    # “sees the sunny side” (extraversion) and “obeys without protest” all load highly together. Their topics differ, 
    # but they share a valence: they’re all good things to say about a child. 
    # When the only thing items share is being desirable, the factor is measuring desirability, or the rater’s overall 
    # impression, rather than a trait.
    # 
    # 2. Does the sign of a loading follow the content, or the scoring key?
    #   This is the decisive test. Reverse-scoring exists so that, within a facet, every item points the same way after scoring. 
    # If MR2 were a real trait, reversed and non-reversed items measuring that trait would load in the same direction once scored.
    # 
    # Here the opposite happened. The sign followed the reversal flag perfectly, 55 out of 55 in children and 61 out of 61 in parents. 
    # Which item was reversed is decided by the questionnaire’s author, not by the child’s personality. 
    # When a factor lines up perfectly with a scoring decision, the factor is about the scoring direction, 
    # meaning the raw tendency to agree.
    # 
    # 3. How big are these factors compared with the rest?
    #   The two factors take about 25% (children) and 31% (parents) of the variance, against about 10% for 
    # the other three combined. So these aren’t minor nuisances; they’re the main structure.
    # 
    # 4. Has this pattern been seen before?
    #   Yes, which is why it’s recognisable. Acquiescence in children’s self-reports, and halo effects in parents’ 
    # and teachers’ ratings, are well documented. Wording factors appear in many questionnaires that mix positively 
    # and negatively worded items. I’m matching a known pattern, not discovering a new one.
    # 
    # Where I overstated: “a reviewer would see it immediately” was too strong. 
    # A reviewer with a psychometrics background would likely spot it, especially the first unrotated factor. 
    # A content reviewer might not. And there’s one alternative explanation that should be ruled out before you present 
    # it as a firm conclusion: wording and content might be confounded. 
    # If De Fruyt’s reversed items happen to be mostly about one kind of behaviour (say, 
    # low effort and problem behaviour), then a “behaviour problems” trait could produce a similar split.
    # 
    # The clean check is within facets. 
    # Take a facet that contains both reversed and non-reversed items, 
    # for example CO_PER (17, 35, 53, 89, 107 and 143 reversed; 71 and 125 not). 
    # If its reversed items load one way on MR2 and its non-reversed items the other way, while measuring the same trait, 
    # then content can’t explain the split, and the wording interpretation is secure. 
    # That’s a short table in R if you want to run it next time.
    # 
    
    
    FA5_PVMAX <- psych::fa(P_ITEMS, nfactors = 5, fm = "minres", rotate = "varimax")
    
    FA5_PVMAX$Vaccounted                                    # variance explained by each factor
    print(FA5_PVMAX$loadings, cutoff = 0.30, sort = TRUE)   # loadings, readable version
    
    
    
    # Parents, five-factor varimax (41.0% of variance): a dominant competence/halo factor (18.4%) merging conscientiousness, 
    # intellect and achievement items, followed by anxiety (7.6%, partly mixed with acquiescence), antagonism/low benevolence (6.9%), 
    # extraversion (5.0%) and altruism (3.1%). Three factors correspond reasonably to NEO N, A and E; 
    # conscientiousness and openness did not separate.
    
    
    
    
    LOAD_C <- psych::fa.sort(FA5_C)$loadings %>% unclass() %>% as.data.frame() %>%
      round(2) %>% tibble::rownames_to_column("ITEM")
    writexl::write_xlsx(LOAD_C, "FA5_children_varimax.xlsx")
    
    
    
    
    summary(FA5_CNOROT$scores[, "MR2"])
    summary(FA5_PNOROT$scores[, "MR2"])
    
    misty::test.welch(ACQ ~ CVPK,
                      data = data.frame(ACQ = FA5_CNOROT$scores[, "MR2"],
                                        CVPK = CHILD_REVERSED$CVPK),
                      descript = TRUE)
    
    
    # For your notes:
    # Children’s scores on the unrotated second (acquiescence) factor did not differ between nutritional groups 
    # (CON M = 0.01, MAL M = −0.01; Welch t = −0.08, df = 67.6, p = .936; 95% CI for the difference −0.48 to 0.44). 
    # Response style is therefore unlikely to bias the group comparisons in the children’s data.
    
    
    misty::test.welch(ACQ ~ CVPK,
                      data = data.frame(ACQ = FA5_PNOROT$scores[, "MR2"],
                                        CVPK = PARENT_REVERSED$CVPK),
                      descript = TRUE)
    

    # For your notes, both samples:
    #   
    #   Scores on the acquiescence factor did not differ by nutritional group in either sample 
    # (children: p = .936; parents: p = .992; 95% CIs for the difference about ±0.47 SD). 
    # The response-style component affects MAL and CON respondents equally, 
    # so it adds noise to the group comparisons rather than biasing them.
    
    
    
    
    #facets
    
    
    
    FA2_C <- psych::fa(C_FACETS, nfactors = 2, fm = "minres", rotate = "varimax")
    FA2_P <- psych::fa(P_FACETS, nfactors = 2, fm = "minres", rotate = "varimax")
    
    print(FA2_C$loadings, cutoff = 0, sort = TRUE)
    print(FA2_P$loadings, cutoff = 0, sort = TRUE)
    
    #For your notes:
      
      # Facet level, two-factor varimax: both samples showed the same two factors (children 50.2%, parents 49.5% of variance). 
      # Factor 1 (engagement) was defined by imagination, extraversion, altruism, achievement and self-confidence facets; 
      # Factor 2 (self-regulation) by perseverance, compliance, concentration and order, with irritability, egoism, dominance and shyness loading negatively. 
      # This resembles the stability/plasticity distinction described above the five-factor level. 
      # Anxiety loaded on Factor 2 in parents (−.57) but weakly in children (−.30).
    
    
    
    
     psych::factor.congruence(FA2_C, FA2_P)
    
    # For your notes:
    # 
    # Tucker’s congruence between the children’s and parents’ two-factor facet solutions was .99 for engagement 
    # and .94 for self-regulation, indicating that the same two-factor structure was recovered from both informants.
    
    
    
    
    
    
    
    run_fa <- function(DSN, NFACT, ROT = "none", METHOD = "minres", SORT = "factor") {
      
      # >>> EXCEL DESTINATION: change this line to write somewhere else <<<
      
      FILE <- "/Users/Kevin/OneDrive/Documents/Excel/HiPIC FA Tests.xlsx"
      
      
      
      
      # 1. The 144 items are always the last 144 columns
      ITEMS  <- DSN %>% select(last_col(offset = 143):last_col())
      PREFIX <- substr(names(ITEMS)[1], 1, 1)                 # "C" or "P"
      
      # 2. Run the analysis: PCA (SAS PROC FACTOR default) or a true factor analysis
      FA <- if (METHOD == "pca") {
        psych::principal(ITEMS, nfactors = NFACT, rotate = ROT)
      } else {
        psych::fa(ITEMS, nfactors = NFACT, fm = METHOD, rotate = ROT)
      }
      
      # 3. Loadings: factors in size order, renamed F1, F2, ...
      L <- unclass(FA$loadings)
      L <- L[, order(-colSums(L^2)), drop = FALSE]
      colnames(L) <- paste0("F", 1:NFACT)
      
      # 4. Item wording: children from the crosswalk, parents from the realignment table
      WORD <- if (PREFIX == "C") {
        HIPIC_CROSSWALK %>% select(Q, Wording = CHILD_Q_WORDING)
      } else {
        PARENT_ALIGNED %>% select(Q = Q_NEW, Wording = PARENT_Q_WORDING)
      }
      
      # 5. Build the table: fixed columns first, loadings last
      OUT <- tibble(Question = as.integer(sub("^[CP](\\d+)_.*", "\\1", rownames(L))),
                    SAS_Name = sub("^[CP]\\d+_([^_]+)_.*", "\\1", rownames(L))) %>%
        left_join(WORD, by = c("Question" = "Q")) %>%
        mutate(Reversed = ifelse(Question %in% REVERSE_ITEMS, "Y", ""),
               Primary  = paste0("F", apply(abs(L), 1, which.max)),
               h2       = round(FA$communality, 3),
               .Max     = apply(abs(L), 1, max)) %>%
        bind_cols(as_tibble(round(L, 3)))
      
      # 6. Sort: by home factor (strongest first), or by item number
      OUT <- if (SORT == "factor") arrange(OUT, Primary, desc(.Max)) else arrange(OUT, Question)
      OUT <- select(OUT, -.Max)
      
      # 6b. Append the variance-explained row at the end
      VAR_ROW <- tibble(Wording = "VAR EXPLAINED") %>%
        bind_cols(as_tibble(t(round(colSums(L^2) / ncol(ITEMS), 3))))
      OUT <- bind_rows(OUT, VAR_ROW)
      
      # 7. Print the headline in the Console
      cat("\n====", deparse(substitute(DSN)), "|", NFACT, "factors |", ROT, "|", METHOD, "====\n")
      print(VAR_ROW %>% select(-Wording))
      
      
      # 8. Write the table to its own sheet in the workbook (skipped with a message if it can't)
      tryCatch({
        SHEET <- paste(PREFIX, NFACT, ROT, METHOD, sep = "_")      # e.g. "P_5_varimax_minres"
        
        wb <- if (file.exists(FILE)) openxlsx::loadWorkbook(FILE) else openxlsx::createWorkbook()
        if (!SHEET %in% names(wb)) openxlsx::addWorksheet(wb, SHEET)
        
        openxlsx::deleteData(wb, SHEET, cols = 1:30, rows = 1:300, gridExpand = TRUE)
        openxlsx::writeData(wb, SHEET, OUT)
        openxlsx::saveWorkbook(wb, FILE, overwrite = TRUE)
        
        cat("Written to sheet", SHEET, "in", basename(FILE), "\n")
      }, error = function(e) {
        cat("Excel export skipped:", conditionMessage(e), "\n")
      })
      
      # 9. Hand back the table (and the full FA result) for inspection
      invisible(list(loadings = OUT, fa = FA))
    }
    
    
    
    
    R1 <- run_fa(CHILD_REVERSED, 5, 'varimax', "minres")
    View(R1$loadings)
    
    
    
    
    
    
    
    
    
    
    
    run_fa <- function(DSN, NFACT, ROT = "none", METHOD = "minres", SORT = "factor") {
      
      # >>> EXCEL DESTINATION: change this line to write somewhere else <<<
      
      FILE <- "/Users/Kevin/OneDrive/Documents/Excel/HiPIC FA Tests.xlsx"
      
      
      
      
      # 1. The 144 items are always the last 144 columns
      ITEMS  <- DSN %>% select(last_col(offset = 143):last_col())
      PREFIX <- substr(names(ITEMS)[1], 1, 1)                 # "C" or "P"
      
      # 2. Run the analysis: PCA (SAS PROC FACTOR default) or a true factor analysis
      FA <- if (METHOD == "pca") {
        psych::principal(ITEMS, nfactors = NFACT, rotate = ROT)
      } else {
        psych::fa(ITEMS, nfactors = NFACT, fm = METHOD, rotate = ROT)
      }
      
      # 3. Loadings: factors in size order, renamed F1, F2, ...
      L <- unclass(FA$loadings)
      L <- L[, order(-colSums(L^2)), drop = FALSE]
      colnames(L) <- paste0("F", 1:NFACT)
      
      # 4. Item wording: children from the crosswalk, parents from the realignment table
      WORD <- if (PREFIX == "C") {
        HIPIC_CROSSWALK %>% select(Q, Wording = CHILD_Q_WORDING)
      } else {
        PARENT_ALIGNED %>% select(Q = Q_NEW, Wording = PARENT_Q_WORDING)
      }
      
      # 5. Build the table: fixed columns first, loadings last
      OUT <- tibble(Question = as.integer(sub("^[CP](\\d+)_.*", "\\1", rownames(L))),
                    SAS_Name = sub("^[CP]\\d+_([^_]+)_.*", "\\1", rownames(L))) %>%
        left_join(WORD, by = c("Question" = "Q")) %>%
        mutate(Reversed = ifelse(Question %in% REVERSE_ITEMS, "Y", ""),
               Primary  = paste0("F", apply(abs(L), 1, which.max)),
               h2       = round(FA$communality, 3),
               .Max     = apply(abs(L), 1, max)) %>%
        bind_cols(as_tibble(round(L, 3)))
      
      # 6. Sort: by home factor (strongest first), or by item number
      OUT <- if (SORT == "factor") arrange(OUT, Primary, desc(.Max)) else arrange(OUT, Question)
      OUT <- select(OUT, -.Max)
      
      # 6b. Append the variance-explained row at the end
      VAR_ROW <- tibble(Wording = "VAR EXPLAINED") %>%
        bind_cols(as_tibble(t(round(colSums(L^2) / ncol(ITEMS), 3))))
      OUT <- bind_rows(OUT, VAR_ROW)
      
      # 7. Print the headline in the Console
      cat("\n====", deparse(substitute(DSN)), "|", NFACT, "factors |", ROT, "|", METHOD, "====\n")
      print(VAR_ROW %>% select(-Wording))
      
      
      # 8. Write the table to its own sheet in the workbook (skipped with a message if it can't)
      tryCatch({
        SHEET <- paste(PREFIX, NFACT, ROT, METHOD, sep = "_")      # e.g. "P_5_varimax_minres"
        
        wb <- if (file.exists(FILE)) openxlsx::loadWorkbook(FILE) else openxlsx::createWorkbook()
        if (!SHEET %in% names(wb)) openxlsx::addWorksheet(wb, SHEET)
        
        openxlsx::deleteData(wb, SHEET, cols = 1:30, rows = 1:300, gridExpand = TRUE)
        openxlsx::writeData(wb, SHEET, OUT)
        openxlsx::saveWorkbook(wb, FILE, overwrite = TRUE)
        
        cat("Written to sheet", SHEET, "in", basename(FILE), "\n")
      }, error = function(e) {
        cat("Excel export skipped:", conditionMessage(e), "\n")
      })
      
      # 9. Hand back the table (and the full FA result) for inspection
      invisible(list(loadings = OUT, fa = FA))
    }
    
    
    
    
    R1 <- run_fa(CHILD_REVERSED, 5, 'varimax', "minres")
    View(R1$loadings)
    
    
    
    ################################
    
    
    
    BASE_WIDE <- BASEC %>%
      group_by(MODEL, VARNAME) %>%
      summarise(
        MAL = sprintf("%.2f ± %.2f", MAL[`_STAT_` == "MEAN"], MAL[`_STAT_` == "STD"]),
        CON = sprintf("%.2f ± %.2f", CON[`_STAT_` == "MEAN"], CON[`_STAT_` == "STD"]),
        t   = round(ALL[`_STAT_` == "TTEST WELCH"], 2),
        p   = round(ALL[`_STAT_` == "PVALUE"], 3),
        .groups = "drop")
    
    
    
    
    
    
    dsn <- c(CHILD_RAW, PARENT_RAW)
    
    
    for (data in dsn) {
      
      make.names( names(dsn), unique= TRUE )
    }
    
    
    names(IBD)<-  make.names( names(IBD), unique = TRUE ) #<<
    
    
    
    
    
    
    
    test <- NULL  
    
    for (x in 0:2) {
      
      LABEL <- c("Males", "Females", "All")[x + 1]        # 0 -> Males, 1 -> Females, 2 -> All
      
      temp <- BASEC %>%
        filter(as.numeric(MODEL) == x) %>%
        group_by(MODEL, VARNAME) %>%
        summarise(
          MAL = sprintf("%.2f ± %.2f", MAL[`_STAT_` == "MEAN"], MAL[`_STAT_` == "STD"]),
          CON = sprintf("%.2f ± %.2f", CON[`_STAT_` == "MEAN"], CON[`_STAT_` == "STD"]),
          t   = round(ALL[`_STAT_` == "TTEST WELCH"], 2),
          p   = round(ALL[`_STAT_` == "PVALUE"], 3),
          .groups = "drop") %>%
        select(-MODEL) %>%                                   # the block's label replaces MODEL
        rename_with(~ paste(LABEL, .x), c(MAL, CON, t, p))   # e.g. "Males MAL", "Females p"
      
#       pad <-  setNames(data.frame(""), paste0("z",x))
      
#        test <- if (x == 0) temp else cbind(test, pad, temp)
       test <- if (x == 0) temp else cbind(test, select(temp, -VARNAME))
    }
    
    
    
    
    (test)
    print(test)
 
    
    
    # a<- c("BENEVOLENCE", 'Altruism', 'Compliance','Dominance','Egocentrism','Irritability',
    #      "CONSCIENTIOUSNESS", 'Achievement','Concentration','Order','Perserverance',
    #      "EMOTIONAL STABILITY", 'Anxiety','Self-Confidence',
    #      "EXTRAVERSION",'Energy','Expressivity','Optimism','Shyness',
    #      "IMAGINATION", 'Creativity','Curiosity','Intellect')
    #  test[ , 1] <- a
    
    
    LABELS <- c(BE     = "BENEVOLENCE",
                BE_ALT = "Altruism",
                BE_COM = "Compliance",
                BE_DOM = "Dominance",
                BE_EGO = "Egocentrism",
                BE_IRR = "Irritability",
                CO     = "CONSCIENTIOUSNESS",
                CO_ACH = "Achievement",
                CO_CNC = "Concentration",
                CO_ORD = "Order",
                CO_PER = "Perseverance",
                ES     = "EMOTIONAL STABILITY",
                ES_ANX = "Anxiety",
                ES_CON = "Self-Confidence",
                EX     = "EXTRAVERSION",
                EX_ENE = "Energy",
                EX_EXP = "Expressiveness",
                EX_OPT = "Optimism",
                EX_SHY = "Shyness",
                IM     = "IMAGINATION",
                IM_CRE = "Creativity",
                IM_CUR = "Curiosity",
                IM_INT = "Intellect")
    
    test$VARNAME <- unname(LABELS[test$VARNAME])
    
    
    
    library(kableExtra)
    
    p_bg <- function(p) ifelse(p < .05, "#d73027", ifelse(p < .10, "orange", "white"))   # background
    p_fg <- function(p) ifelse(p < .05, "white", "black")                                 # text colour
    
    
    TAB <- test %>%
      kbl(col.names = c("Variable", rep(c("MAL", "CON", "t", "p-Value"), 3)),
          align = c("l", rep("c", 12))) %>%
      kable_styling(full_width = FALSE, font_size = 12) %>%
      add_header_above(c(" " = 1, "Males" = 4, "Females" = 4, "All" = 4)) %>%
      column_spec(c(1, 5, 9), border_right = TRUE)
    
    for (j in c(5, 9, 13)) {
      TAB <- column_spec(TAB, j, background = p_bg(test[[j]]), color = p_fg(test[[j]]))
    }
    
    TAB
    
    
    
    
    names(test)
