# =======================================================================================================
# ========================================= PARAMETERS ==================================================
# =======================================================================================================

params <- yaml.load_file('hparams.yaml')

NOTES = params$relevant.files$grades
LABS_IGNORE = params$relevant.files$ignorelab
CUR_UPDATE = params$relevant.files$currupdate
CUR_NECS = params$relevant.files$currnecessity
LM_MODELS = params$models.dir$regression
CL_MODELS = params$models.dir$classification
ABS_MODELS = params$models.dir$absolute

PLOTS_DIR_REG <- params$plots.dir$regression
PLOTS_DIR_CLA <- params$plots.dir$classification
PLOTS_DIR_ABS <- params$plots.dir$absolute

# =======================================================================================================
# ========================================= DATA FILTERS ================================================
# =======================================================================================================

labs.omit <- function(data){
  subjs.omit <- c(unique(grep('+',data$Codigo.Asignatura,value = TRUE,fixed = TRUE)),
                  unique(grep('-',data$Codigo.Asignatura,value = TRUE,fixed = TRUE)))
  data.omitted <- data[ !(data$Codigo.Asignatura %in% subjs.omit), ] # SUBJECTS OMITED
  return(droplevels(data.omitted))
}

spaceSubject.filt <- function(data){
  levels(data$Codigo.Asignatura) <- gsub(' ','',levels(data$Codigo.Asignatura))
  return(na.omit(data))
}

subjects.homolog <- function(data){
  curriculum <- read.csv(CUR_UPDATE,header = TRUE)
  for (i.cur in 1:nrow(curriculum)) {
    data[grep(curriculum$HOMOLOGS[i.cur],data$Codigo.Asignatura),]$Codigo.Asignatura <- curriculum$SUBJECT[i.cur]
  }
  return(droplevels(data))
}

# =======================================================================================================
# ======================================= MODELS SAVE/LOAD ==============================================
# =======================================================================================================

save.model <- function(model,file.name.path){
  file.name.path <- gsub('[^[:alnum:][:blank:]+_./\\-]','',file.name.path) # REMOVE SPECIAL CHARS EXCEPT +_./\\-
  print(file.name.path)
  saveRDS(model, file = file.name.path)
}

load.models <- function(files.path){
  models.files <- list.files(path = files.path)
  models <- list()
  for (file.model in models.files) {
    model.name <- gsub(".rds","",file.model)
    models[[model.name]] <- load.model(paste(files.path,file.model,sep = ""))
  }
  return(models)
}

load.model <- function(file.model){
  return(readRDS(file.model))
}