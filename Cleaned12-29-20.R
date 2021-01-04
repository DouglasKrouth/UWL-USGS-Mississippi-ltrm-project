library(readxl)
data = read_excel("C:/Users/cashe/OneDrive/Desktop/Data Science/Mississippi River analysis/ltrm_water_dataexl.xlsx")
keep = c('TP','TPQF','TN','TNQF','SS','SSQF',
          'TURB','TURBQF','WDP',
          'TEMP','TEMPQF','DO','DOQF','COND',
          'CONDQF','VEL','VELQF','FLDEAST',
          'FLDNORTH','PROJCD','FLDNUM','DATE',
          'LOCATCD','STRATUM','CHLcal','SECCHI','SECCHIQF')

cat("Before filtering:",dim(data))
data = data[,(names(data) %in% keep)]
cat("After filtering columns:",dim(data))

# Now filter sample design
data = data[data$PROJCD=='M-',]
cat("After filtering sample design:",dim(data))

# Now filter pool 13
data = data[data$FLDNUM == 3,]
cat("After filtering pool 13:",dim(data))

# Now adding a year column
data$YEAR = format(data$DATE,format = "%Y")
cat("After adding year column:",dim(data))

# Now adding a time code column
data$TIMECODE = substr(data$LOCATCD,4,4)
cat("After adding a timecode column:",dim(data))

# Now filtering by backwater lakes
data = data[data$STRATUM==3,]
cat("After filtering by backwater lakes:",dim(data))

QFcols = c('TPQF','TNQF','SSQF','TURBQF','TEMPQF','DOQF','CONDQF','VELQF','SECCHIQF')
for (col in QFcols){
  data = data[(is.na(data[col])), ]
}
cat("After filtering by non-blank QF codes:",dim(data))

drop = c('PROJCD','FLDEAST','FLDNORTH','TPQF','TNQF','SSQF','TURBQF','TEMPQF','DOQF',
          'CONDQF','VELQF','SECCHIQF')
data = data[,!(names(data) %in% drop)]
cat("After filtering empty columns:",dim(data))

continuousv = c('TP','TN','TEMP','DO','TURB','COND','VEL','SS','WDP','CHLcal','SECCHI')
for (col in continuousv){
  data = data[!(is.na(data[col])), ]
}
cat("After filtering rows with empty entries:",dim(data))







