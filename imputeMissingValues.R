imputeMissingValues <- function(dataIn) {
  
  # Sum of NAs for each column
  colSums(is.na(dataIn))
  
  # Highest numerical correlation with Age using Pearson Correlation Coefficient ----
  dataNum <- unlist(lapply(dataIn, is.numeric))
  dataNum1 <- dataIn[, dataNum]
  
  cor <- abs(apply(dataNum1,
            2,
            function(col)cor(col, dataNum1$Age, method = "pearson", use = "complete.obs"))) %>%
    sort(TRUE)
  cor1 = names(cor)
  cor1 = cor1[2]
  
  # NA Age: Replace NA age with median Age of Pclass and Sex ----
  ageMedian <- dataIn %>%
    group_by(Pclass, Sex) %>%
    summarise(AgeMedian = median(Age, na.rm = TRUE)) %>%
    ungroup
  
  dataIn <- dataIn %>%
    left_join(ageMedian) %>%
    mutate(Age = if_else(is.na(Age), AgeMedian, Age)) %>%
    select(-AgeMedian)
  
  # NA Embarked: find most common Embarked result for 1st class females in Cabin B
  # Missing values are female, first class, cabin B passengers ----
  proxyEmbarkedMissing <- dataIn %>%
    filter(Pclass==1, Sex=="female", str_detect(Cabin, "B"))
  
  embarkedMode <- unique(dataIn$Embarked)[which.max(tabulate(match(dataIn$Embarked, unique(dataIn$Embarked))))]
  
  dataIn <- dataIn %>%
    mutate(Embarked = if_else(is.na(Embarked), embarkedMode, Embarked))
  
  # NA Cabin: ----
  dataIn <- dataIn %>%
    mutate(Deck = gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',Cabin,perl = TRUE),
           Deck = if_else(is.na(Deck),
                           case_when(Pclass == 1 ~ "ABC",
                                     Pclass == 2 ~ "DE",
                                     Pclass == 3 ~ "FG"),
                           Deck),
           Cabin = if_else(!is.na(Cabin), Cabin, Deck))%>%
    select(-Deck)
  
  return(dataIn)
  
}