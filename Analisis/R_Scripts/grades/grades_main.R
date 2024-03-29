source("Analisis/R_Scripts/grades/grades_packages.R")
source("Analisis/R_Scripts/grades/grades_utils.R")

source("Analisis/R_Scripts/grades/Classification/classification.R")
source("Analisis/R_Scripts/grades/Regression/regression.R")

source("Analisis/R_Scripts/grades/approach_selection.R")
source("Analisis/R_Scripts/grades/approach_benefit.R")

wd <- commandArgs(trailingOnly = TRUE)
root.dir <- paste(wd, collapse = ' ')
setwd(root.dir)
getwd()
# =====================================================================================================================
# =========================================== DATA ADQUISITION ========================================================
# =====================================================================================================================

paste('Loading Grades:',NOTES)
data.grades <- read.csv(NOTES, header = TRUE)

# IGNORE LAB ASIGNATURES
if (LABS_IGNORE) {
  data.grades <- labs.omit(data.grades)
}
# HOMOLOGATE DATA IF NECESSARY
if (CUR_NECS) {
  data.grades <- subjects.homolog(data.grades)
}

# REMOVE UNECESSARY SPACES ON SUBJECTS CODS
data.grades <- spaceSubject.filt(data.grades)

# subjects.consider <- unique(data.grades[data.grades$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
subjects.consider <- unique(data.grades[data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS') ,]$Codigo.Asignatura)
subjects.consider <- subjects.consider[!laply( subjects.consider, function(val){ return( grepl("\\(", val) || grepl("\\)", val) ) } ) ]

years.consider <- strtoi(unique(sub("-.*","",droplevels(data.grades[data.grades$Codigo.Asignatura %in% subjects.consider,]$Periodo.Academico))))
years.consider <- c(years.consider[-c(1,2)],max(years.consider)+1)
years.consider <- tail(years.consider, n = 1)

# =====================================================================================================================
# ============================================ MODELS TRAINING ========================================================
# =====================================================================================================================
set.seed(123)
# asigntatures.consider[151:length(asigntatures.consider)]
'Regression Training ...'
reg.models <- regress.bestModel.train(allData = data.grades,asignatures = subjects.consider, allowed.years = years.consider)
reg.models <- Filter(length,reg.models) # EMPTY MODELS REMOVED
'Classification Training ...'
cla.models <- classif.bestModel.train(allData = data.grades,asignatures = subjects.consider, allowed.years = years.consider)
cla.models <- Filter(length,cla.models) # EMPTY MODELS REMOVED


# =====================================================================================================================
# =================================== BEST LEARNING APPROACH SELECTION ================================================
# =====================================================================================================================

'Best Model Selection...'
best.models <- abs.models.selection(data.grades, reg.models, cla.models, subjects.consider, years.consider)

# =====================================================================================================================
# ====================================== MODELS & APPROACHS BENEFIT ===================================================
# =====================================================================================================================
#'Final Plots..'
#data.grades <- data.grades[data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS'),]

#abs.bestModel.benefit.general(data.grades,subjects.consider)
# abs.bestModel.benefit.asig(data.grades,subjects.consider)

# regress.bestModel.benefit.general(data.grades,subjects.consider)
# regress.bestModel.benefit.asig(data.grades,subjects.consider)
# 
# classif.bestModel.benefit.general(data.grades,subjects.consider)
# classif.bestModel.benefit.asig(data.grades,subjects.consider)
