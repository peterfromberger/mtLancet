library(ggstatsplot)
#library(descsuppRplots) # remotes::install_git("https://gitlab.gwdg.de/aleha/descsuppRplots", dependencies = FALSE)
library(nparcomp)
library(dplyr)
library(readxl)
library(ggplot2)
library(magrittr)
library(here) # for better management of resources and folders

verbose <- FALSE

## Settings
export_date <- as.Date("2024-11-22")

## simulate group data
simulate_only <- FALSE

## assumed structure:
## <inDir>/<export_date>/SecuTrial/mnpszum3cvtrq1...
## <inDir>/<export_date>/myTabu_database/questionnaires_...

## setup project directories
projectDir <- file.path(here::here("_data/MBSB"))

imgDir   <- file.path(projectDir, "img")
cacheDir <- file.path(projectDir, "cache")

dataDir  <- file.path(projectDir, "data")
inDir    <- file.path(dataDir,    "input")
procDir  <- file.path(dataDir,    "proc")
outDir   <- file.path(dataDir,    "output")

exportDir <- file.path(inDir, export_date)
exportDirSecuTrial <- file.path(exportDir, "SecuTrial")
if (verbose) file.exists(exportDirSecuTrial)
if (verbose) dir(exportDirSecuTrial)
exportDirmyTabu <- file.path(exportDir, "mtDatabase_26112024_FINAL")
if (verbose) file.exists(exportDirmyTabu)
if (verbose) dir(exportDirmyTabu)

## read data
## #########

## unfortinately the data are not read into utf-8
transcode2utf8 <- function(x)
{
  x %>%
    mutate_if(is.character, function(x) iconv(x, from = "latin1", to = "UTF-8"))
}


if (verbose) dir(exportDirSecuTrial)

## centers
center <- read.csv(file.path(exportDirSecuTrial, "centres_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) center
## not very informative

## casenodes
cn <- read.csv(file.path(exportDirSecuTrial, "casenodes_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) cn
## not very informative either

## visitplan
visitplan <- read.csv(file.path(exportDirSecuTrial, "visitplan_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
visitplan <- visitplan[visitplan$mnpvslbl==visitplan$mnpvslbl[1],]   # current version
if (verbose) visitplan
## notvery informative either

## forms list
forms <- read.csv(file.path(exportDirSecuTrial, "forms_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
forms <- forms[forms$mnpvslbl==forms$mnpvslbl[1],]   # current version
if (verbose) forms %>% print(n = Inf)

## items
items <- read.csv(file.path(exportDirSecuTrial, "items_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) items %>% print(n = 100)

## function to rename (and optionally subset) columns in each form
## according to entries in forms and items
getItems <- function(fname, frms = forms, itms = items)
{
  frms %>%
    filter(formname == fname) %>%
    .$formid  %>%
    {filter(itms, formid == .)} %>%
    filter(itemtype != "Layout-Dummy") %>%
    dplyr::select(ffcolname, fglabel, fflabel)
}

renameCols <- function(form, fname, frms = forms, itms = items, subset = FALSE) {
  nameMapping <- getItems(fname, frms = frms, itms = itms)
  nameMapping <- nameMapping %>% mutate(fgfflabel = ifelse(fflabel == fglabel, fflabel, paste(fglabel, fflabel, sep = " --- ")))
  if (any(duplicated(nameMapping$fgfflabel))) {
    idx <- which(nameMapping$fgfflabel %in% nameMapping$fgfflabel[duplicated(nameMapping$fgfflabel)])
    nameMapping$fgfflabel[idx] <- paste0(nameMapping$fglabel[idx], " --- ",
                                         "(", nameMapping$ffcolname[idx], ") ",
                                         nameMapping$fflabel[idx])
  }
  nameMappingV <- nameMapping$fgfflabel
  names(nameMappingV) <- nameMapping$ffcolname
  idx <- colnames(form) %in% names(nameMappingV)
  colnames(form)[idx] <-
    nameMappingV[colnames(form)[idx]]
  if (subset)
    form <- form %>% dplyr::select(ID = mnpaid, one_of(nameMappingV))
  return(form)
}

## forms
## 1 mnpszum3strata1     "Strata"
strata_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3strata1_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) strata_full
if (verbose) strata_full %>% head %>% as.data.frame
if (verbose) strata_full %>% nrow
if (verbose) strata_full %>% renameCols("Strata")
strata <- strata_full %>% renameCols("Strata", subset = TRUE)
if (verbose) strata
if (verbose) strata %>% colnames
if (verbose) strata %>% head %>% as.data.frame
if (verbose) strata %>% dplyr::select(`8.4 Rückfallrisiko nach Static-99: --- Static-Score:`,
                                      `8.4 Rückfallrisiko nach Static-99: --- Modified Static (Score zur Randomisierung):`) %>%
  print(n = Inf)
strata <-
  strata %>%
  mutate(`8.4 Rückfallrisiko nach Static-99: --- Modified` =
           !is.na(`8.4 Rückfallrisiko nach Static-99: --- Modified Static (Score zur Randomisierung):`),
         `8.4 Rückfallrisiko nach Static-99: --- Static-Score:` =
           ifelse(`8.4 Rückfallrisiko nach Static-99: --- Modified`,
                  `8.4 Rückfallrisiko nach Static-99: --- Modified Static (Score zur Randomisierung):`,
                  `8.4 Rückfallrisiko nach Static-99: --- Static-Score:`)) %>%
  dplyr::select(-`8.4 Rückfallrisiko nach Static-99: --- Modified Static (Score zur Randomisierung):`)
##  2 mnpszum3rando       "Randomisierung"
set.seed(838)
rando_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3rando_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
## HERE WE SIMULATE THE STUDY ARM
if (simulate_only) rando_full <- rando_full %>% mutate(randtreat = sample(c("Placebo", "Intervention"), n(), replace = TRUE))
if (verbose) rando_full
if (verbose) rando_full %>% head %>% as.data.frame
if (verbose) rando_full$randdone %>% table(exclude = NULL)
if (verbose) rando_full$randtreat %>% table(exclude = NULL)
if (verbose) rando_full %>% nrow
rando <- rando_full %>% renameCols("Randomisierung", subset = TRUE)
if (verbose) rando
rando <-
  rando %>%
  dplyr::rename(Randomisiert = `1. Kann der Patient randomisiert werden:`,
                Randomisierungsdatum = `1. Kann der Patient randomisiert werden: --- 1a. Wenn \\Ja\\, Datum der Randomisierung:`,
                Grund_Nichtrandomisierung = `1. Kann der Patient randomisiert werden: --- 1b. Wenn \\Nein\\, bitte Grund angeben:`,
                treatment = `2. Randomisierung durchführen:`)
rando <-
  rando %>%
  mutate(Randomisierungsdatum = as.character(Randomisierungsdatum),
         Randomisierungsdatum = as.Date(Randomisierungsdatum, format = "%Y%m%d")) %>%
  filter(!ID %in% c("myTabu243", "myTabu340")) # , "myTabu154", "myTabu367", Klienten 'myTabu243' und 'myTabu340' erfüllen NICHT die Einschlusskriterien.
if (verbose) rando %>% colnames
if (verbose) rando %>% head %>% as.data.frame
##  3 mnpszum3krit        "1. Ein- und Ausschlusskriterien"
krit_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3krit_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) krit_full
if (verbose) krit_full %>% head %>% as.data.frame
if (verbose) krit_full %>% nrow
krit <- krit_full %>% renameCols("1. Ein- und Ausschlusskriterien", subset = TRUE)
if (verbose) krit
if (verbose) krit %>% colnames
if (verbose) krit %>% head %>% as.data.frame
##  4 mnpszum3teilnehmer  "2. Teilnehmender oder Nichtteilnehmender"
teilnehmer_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3teilnehmer_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) teilnehmer_full
if (verbose) teilnehmer_full %>% head %>% as.data.frame
if (verbose) teilnehmer_full$teilmod %>% table(exclude = NULL)
if (verbose) teilnehmer_full %>% nrow
teilnehmer <- teilnehmer_full %>% renameCols("2. Teilnehmender oder Nichtteilnehmender", subset = TRUE)
if (verbose) teilnehmer
if (verbose) teilnehmer %>% colnames
if (verbose) teilnehmer %>% head %>% as.data.frame
##  5 mnpszum3einver      "3. Einverständniserklärung"
einver_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3einver_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) einver_full
if (verbose) einver_full %>% head %>% as.data.frame
if (verbose) einver_full$einvererklaerung %>% table(exclude = NULL)
if (verbose) einver_full$einverschweig %>% table(exclude = NULL)
if (verbose) einver_full$einverbarung %>% table(exclude = NULL)
if (verbose) einver_full %>% nrow
einver <- einver_full %>% renameCols("3. Einverständniserklärung", subset = TRUE)
if (verbose) einver
if (verbose) einver %>% colnames
if (verbose) einver %>% head %>% as.data.frame
##  6 mnpszum3demodat     "4. Demographische Daten"
demodat_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3demodat_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) demodat_full
if (verbose) demodat_full %>% head %>% as.data.frame
if (verbose) demodat_full$demobund %>% table(exclude = NULL)
if (verbose) demodat_full$demobunda %>% table(exclude = NULL)
if (verbose) demodat_full$demogend %>% table(exclude = NULL)
if (verbose) demodat_full$static1_1 %>% table(exclude = NULL)
if (verbose) demodat_full$demobez %>% table(exclude = NULL)
if (verbose) demodat_full$static2_1 %>% table(exclude = NULL)
if (verbose) demodat_full %>% nrow
demodat <- demodat_full %>% renameCols("4. Demographische Daten", subset = TRUE)
if (verbose) demodat
if (verbose) demodat %>% colnames
if (verbose) demodat %>% head %>% as.data.frame
if (verbose) demodat %>% tail %>% as.data.frame
demodat <-
  demodat %>%
  mutate(`4.2 Bundesland:` =
           ifelse(`4.2 Bundesland:` == "Anderes Bundesland",
                  `4.2 Bundesland: --- 4.2a Wenn \\Anderes Bundesland\\ welches:`,
                  `4.2 Bundesland:`)) %>%
  dplyr::select(-`4.2 Bundesland: --- 4.2a Wenn \\Anderes Bundesland\\ welches:`)
if (verbose) demodat %>% head %>% as.data.frame
if (verbose) demodat %>% tail %>% as.data.frame

##  7 mnpszum3indexdelikt "5. Indexdelikt"
indexdelikt_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3indexdelikt_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) indexdelikt_full
if (verbose) indexdelikt_full %>% head %>% as.data.frame
if (verbose) indexdelikt_full$index1 %>% table(exclude = NULL)
if (verbose) indexdelikt_full$index2 %>% table(exclude = NULL)
if (verbose) indexdelikt_full$index3 %>% table(exclude = NULL)
if (verbose) indexdelikt_full$index4 %>% table(exclude = NULL)
if (verbose) indexdelikt_full$index5 %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexgrund %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexopfer %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexunbek %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexmann %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexirgend %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexmehropf %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexirgendopf %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indexnichtfam %>% table(exclude = NULL)
if (verbose) indexdelikt_full$indxscore %>% table(exclude = NULL)
if (verbose) indexdelikt_full %>% nrow
indexdelikt <- indexdelikt_full %>% renameCols("5. Indexdelikt", subset = TRUE)
if (verbose) indexdelikt
if (verbose) indexdelikt %>% colnames
if (verbose) indexdelikt %>% head %>% as.data.frame
indexdelikt <- indexdelikt %>% dplyr::rename(`Indexdelikt Gesamtscore Formular` = `Gesamtscore Formular:`)
##  8 emnpszum3indexdat3  "Indexdatumsangaben4"
##  9 emnpszum3indexdat2  "Indexdatumsangaben3"
## 10 emnpszum3indexdat1  "Indexdatumsangaben2"
## 11 emnpszum3indexdat   "Indexdatumsangaben"
indexdat <- read.csv(file.path(exportDirSecuTrial, "emnpszum3indexdat_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) indexdat
if (verbose) indexdat %>% head %>% as.data.frame
if (verbose) indexdat %>% nrow
##indexdat <- indexdat %>% renameCols("Indexdatumsangaben", subset = TRUE)
##if (verbose) indexdat
## 12 mnpszum3vorstrafen  "6. Vorstrafen"
vorstrafen_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3vorstrafen_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) vorstrafen_full
if (verbose) vorstrafen_full %>% head %>% as.data.frame
if (verbose) vorstrafen_full$vsanzahl %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vstat1 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vstat2 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vstat3 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vstat4 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vstat5 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsnosex %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurt %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbno %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurta %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtb %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbc %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbc1 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbcno %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbcka %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbd %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbdno %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbdka %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtbd1 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$anklageuweisscore %>% table(exclude = NULL)
if (verbose) vorstrafen_full$verurteilscore %>% table(exclude = NULL)
if (verbose) vorstrafen_full$anklagescore %>% table(exclude = NULL)
if (verbose) vorstrafen_full$gesamtscore %>% table(exclude = NULL)
if (verbose) vorstrafen_full$gesamtscore1 %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsverurtohne %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsbesitz %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vsurtalt %>% table(exclude = NULL)
if (verbose) vorstrafen_full$vorstrafenscore %>% table(exclude = NULL)
if (verbose) vorstrafen_full %>% nrow
if (verbose) getItems("6. Vorstrafen") %>% print(n = Inf)
vorstrafen <- vorstrafen_full %>% renameCols("6. Vorstrafen", subset = TRUE)
if (verbose) vorstrafen
if (verbose) vorstrafen %>% colnames
if (verbose) vorstrafen %>% head %>% as.data.frame
vorstrafen <- vorstrafen %>% dplyr::rename(`Vorstrafen Gesamtscore Formular` = `Gesamtscore Formular:`)
## 13 mnpszum3interview   "7. Interview mit Klienten"
interview_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3interview_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) interview_full
if (verbose) interview_full %>% head %>% as.data.frame
if (verbose) interview_full$interalt %>% table(exclude = NULL)
if (verbose) interview_full$interbez %>% table(exclude = NULL)
if (verbose) interview_full$interverwandt %>% table(exclude = NULL)
if (verbose) interview_full$interunbek %>% table(exclude = NULL)
if (verbose) interview_full$interunbekopf %>% table(exclude = NULL)
if (verbose) interview_full$interindexhaft %>% table(exclude = NULL)
if (verbose) interview_full$interaltentl %>% table(exclude = NULL)
if (verbose) interview_full$interbehandl %>% table(exclude = NULL)
if (verbose) interview_full$interbetreu %>% table(exclude = NULL)
if (verbose) interview_full$interviewscore %>% table(exclude = NULL)
if (verbose) interview_full$gesamtscoreinterview %>% table(exclude = NULL)
if (verbose) interview_full$gesamtscoreintervie2 %>% table(exclude = NULL)
if (verbose) interview_full %>% nrow
interview <- interview_full %>% renameCols("7. Interview mit Klienten", subset = TRUE)
if (verbose) interview
if (verbose) interview %>% colnames
if (verbose) interview %>% head %>% as.data.frame
interview <- interview %>% dplyr::rename(`Interview Gesamtscore Formular` = `Gesamtscore Formular:`)
## 14 mnpszum3nichtteil   "11. Fragebogen zu Gründen für eine Nicht-Teilnahme bei der Online-Intervention"
nichtteil_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3nichtteil_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) nichtteil_full
if (verbose) nichtteil_full %>% head %>% as.data.frame
if (verbose) nichtteil_full$nichtteil1 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil2 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil3 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil4 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil5 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil6 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil7 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil8 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil9 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil10 %>% table(exclude = NULL)
if (verbose) nichtteil_full$nichtteil11 %>% table(exclude = NULL)
if (verbose) nichtteil_full %>% nrow
nichtteil <- nichtteil_full %>% renameCols("11. Fragebogen zu Gründen für eine Nicht-Teilnahme bei der Online-Intervention", subset = TRUE)
if (verbose) nichtteil
if (verbose) nichtteil %>% colnames
if (verbose) nichtteil %>% head %>% as.data.frame

# new version of static:
# teilnehmer_full demodat_full strata_full indexdelikt_full vorstrafen_full interview_full

s1 <- teilnehmer_full %>% dplyr::select(mnpaid, teilmod) %>%
  full_join(demodat_full, by = c("mnpaid" = "mnpaid")) %>%
  dplyr::select(mnpaid, teilmod, static1_1, static2_1)

s2 <- teilnehmer_full %>% dplyr::select(mnpaid, teilmod) %>%
  full_join(indexdelikt_full, by = c("mnpaid" = "mnpaid")) %>%
  dplyr::select(mnpaid, teilmod, static3, static8_1, static9_1, static10_1, index3, index1, index2, index4)

s2 = dplyr::select(s2, mnpaid, static3, static8_1, static9_1, static10_1, index3, index1, index2, index4)

s3 <- teilnehmer_full %>% dplyr::select(mnpaid, teilmod) %>%
  full_join(vorstrafen_full, by = c("mnpaid" = "mnpaid")) %>%
  dplyr::select(mnpaid, teilmod, static6, static4, static5_1, static5_2, static5_3, static5_7, static5_8, static5_9, static7, vsbesitz)
s3 = dplyr::select(s3, mnpaid, static6, static4, static5_1, static5_2, static5_3, static5_7, static5_8, static5_9, static7, vsbesitz)

s4 <- teilnehmer_full %>% dplyr::select(mnpaid, teilmod) %>%
  full_join(interview_full, by = c("mnpaid" = "mnpaid")) %>%
  dplyr::select(mnpaid, teilmod, static1_2, static2_2, static8_2, static9_2, static10_2)

s4 = dplyr::select(s4, mnpaid, static1_2, static2_2, static8_2, static9_2, static10_2)

s5 <- teilnehmer_full %>% dplyr::select(mnpaid, teilmod) %>%
  full_join(strata_full, by = c("mnpaid" = "mnpaid")) %>%
  dplyr::select(mnpaid, strataindex , gesamtscore, gesamtscore2)

static = left_join(s1, s2, by=c('mnpaid'='mnpaid'))
static = left_join(static, s3, by=c('mnpaid'='mnpaid'))
static = left_join(static, s4, by=c('mnpaid'='mnpaid'))
static = left_join(static, s5, by=c('mnpaid'='mnpaid'))
static = dplyr::select(static, mnpaid, teilmod, static1_1, static1_2, static2_1, static2_2,
                static3, static4, static5_1, static5_2, static5_3, static5_7,
                static5_8, static5_9, static6, static7, static8_1, static8_2,
                static9_1, static9_2, static10_1, static10_2,
                index3, index1, index2, index4,
                gesamtscore, gesamtscore2,
                vsbesitz)

static$static1_2 <- as.numeric(static$static1_2)
static$static2_2 <- as.numeric(static$static2_2)
static$static8_2 <- as.numeric(static$static8_2)
static$static9_2 <- as.numeric(static$static9_2)
static$static10_2 <- as.numeric(static$static10_2)
static %<>%
  dplyr::mutate_at(3:22, ~ replace(., is.na(.), 0))

static <- static %>% mutate(static1_calc = static1_1 + static1_2)
static <- static %>% mutate(static2_calc = static2_1 + static2_2)
static <- static %>% mutate(static8_calc = static8_1 + static8_2)
static <- static %>% mutate(static9_calc = static9_1 + static9_2)
static <- static %>% mutate(static10_calc = static10_1 + static10_2)
static <- static %>% mutate(static5a = static5_1 + static5_2 + static5_3)
static <- static %>% mutate(static5b = static5_7 + static5_8 + static5_9)
static <- static %>% mutate(static5_calc = pmax(static5a, static5b))
static <- dplyr::select(static, mnpaid, teilmod,
                 static1_calc, static2_calc, static3, static4, static5_calc, static6, static7, static8_calc, static9_calc, static10_calc,
                 index3, index1, index2, index4,
                 gesamtscore, gesamtscore2, index3,
                 vsbesitz)

# variable mit indexdelikt (da strataindex missings hat)
static <- static %>% mutate(strataindex = case_when(index4 == "Ja" ~ "§184b StGB")) %>%
  mutate(strataindex = case_when(index1 == "Ja" ~ "§§176ff StGB and mixed", TRUE ~ strataindex)) %>%
  mutate(strataindex = case_when(index2 == "Ja" ~ "§§176ff StGB and mixed", TRUE ~ strataindex)) %>%
  mutate(strataindex = case_when(index3 == "Ja" ~ "§§176ff StGB and mixed", TRUE ~ strataindex))

# codierungsfehler von static-item 7
static <- static %>% mutate(static7_calc = case_when(index3 == "Ja" ~ 1, TRUE ~ static7)) %>%
  mutate(static7_calc = case_when(vsbesitz == "Ja" ~ 1, TRUE ~ static7))

static$static3_calc <- static$static3
static$static4_calc <- static$static4
static$static6_calc <- static$static6

# static-99 und modified static-99
static <- static %>% rowwise() %>% mutate(static99_modified_calc = sum(c(static1_calc, static2_calc, static3_calc, static4_calc, static5_calc, static6_calc, static7_calc))) %>% ungroup()
static <- static %>% rowwise() %>% mutate(static99_calc = sum(c(static1_calc, static2_calc, static3_calc, static4_calc, static5_calc, static6_calc, static7_calc, static8_calc, static9_calc, static10_calc))) %>% ungroup()
static <- static %>% mutate(static99_calc = case_when(strataindex == "§184b StGB" ~ NA, TRUE ~ static99_calc))

static$static99_calc <- as.numeric(as.character(static$static99_calc))
static$static99_calc_factor <- cut(static$static99_calc, breaks = c(-Inf, 1, 3, 5, Inf),
                                   labels = c("0-1 (geringes Risiko)", "2-3 (gering-moderates Risiko)", "4-5 (moderates-hohes Risiko)", "6+ (hohes Risiko)"),
                                   right = TRUE)

static$static99_modified_calc <- as.numeric(as.character(static$static99_modified_calc))
static$static99_modified_calc_factor <- cut(static$static99_modified_calc, breaks = c(-Inf, 1, 3, 5, Inf),
                                            labels = c("0-1 (geringes Risiko)", "2-3 (gering-moderates Risiko)", "4-5 (moderates-hohes Risiko)", "6+ (hohes Risiko)"),
                                            right = TRUE)
static %<>% rename(ID = mnpaid)

## 15 mnpszum3atq1        "10. ATQ"
atq1_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3atq1_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) atq1_full
if (verbose) atq1_full %>% head %>% as.data.frame
if (verbose) atq1_full$atq1 %>% table(exclude = NULL)
if (verbose) atq1_full$atq2 %>% table(exclude = NULL)
if (verbose) atq1_full$atq3 %>% table(exclude = NULL)
if (verbose) atq1_full$atq4 %>% table(exclude = NULL)
if (verbose) atq1_full$atq5 %>% table(exclude = NULL)
if (verbose) atq1_full$atq6 %>% table(exclude = NULL)
if (verbose) atq1_full$atq7 %>% table(exclude = NULL)
if (verbose) atq1_full$atq8 %>% table(exclude = NULL)
if (verbose) atq1_full$atq9 %>% table(exclude = NULL)
if (verbose) atq1_full$atq10 %>% table(exclude = NULL)
if (verbose) atq1_full$atq11 %>% table(exclude = NULL)
if (verbose) atq1_full$atq12 %>% table(exclude = NULL)
if (verbose) atq1_full$atq13 %>% table(exclude = NULL)
if (verbose) atq1_full$atq14 %>% table(exclude = NULL)
if (verbose) atq1_full$atq15 %>% table(exclude = NULL)
if (verbose) atq1_full$atq16 %>% table(exclude = NULL)
if (verbose) atq1_full$atq17 %>% table(exclude = NULL)
if (verbose) atq1_full$atq18 %>% table(exclude = NULL)
if (verbose) atq1_full$atq19 %>% table(exclude = NULL)
if (verbose) atq1_full$atq20 %>% table(exclude = NULL)
if (verbose) atq1_full$atq21 %>% table(exclude = NULL)
if (verbose) atq1_full$atq22 %>% table(exclude = NULL)
if (verbose) atq1_full$atq23 %>% table(exclude = NULL)
if (verbose) atq1_full$atq24 %>% table(exclude = NULL)
if (verbose) atq1_full$atq25 %>% table(exclude = NULL)
if (verbose) atq1_full$atq26 %>% table(exclude = NULL)
if (verbose) atq1_full %>% nrow
atq1 <- atq1_full %>% renameCols("10. ATQ", subset = TRUE)
if (verbose) atq1
if (verbose) atq1 %>% colnames
if (verbose) atq1 %>% head %>% as.data.frame
## 16 mnpszum3cvtrq1      "9. CVTRQ"
cvtrq1_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3cvtrq1_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) cvtrq1_full
if (verbose) cvtrq1_full %>% head %>% as.data.frame
if (verbose) cvtrq1_full$cvtrq1 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq2 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq3 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq4 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq5 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq6 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq7 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq8 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq9 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq10 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq11 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq12 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq13 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq14 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq15 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq16 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq17 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq18 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq19 %>% table(exclude = NULL)
if (verbose) cvtrq1_full$cvtrq20 %>% table(exclude = NULL)
if (verbose) cvtrq1_full %>% nrow
cvtrq1 <- cvtrq1_full %>% renameCols("9. CVTRQ", subset = TRUE)
if (verbose) cvtrq1
if (verbose) cvtrq1 %>% colnames
if (verbose) cvtrq1 %>% head %>% as.data.frame
## 17 mnpszum3bewaehr     "12. Fragebogen für Bewährungshelfende Eingangsbefragung"
bewaehr_full <- read.csv(file.path(exportDirSecuTrial, "mnpszum3bewaehr_SZUM3_20240424-130854.csv"), fileEncoding = "Latin1") #%>% transcode2utf8
if (verbose) bewaehr_full
if (verbose) bewaehr_full %>% head %>% as.data.frame
if (verbose) bewaehr_full$bewaehrbefrag1 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag1a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag1b %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag2 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag2a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag3 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag4 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag4a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag5 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag6 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag7 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag8 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag8a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag9 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag9a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag10 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag11 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag12 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag13 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag14 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag14a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag15 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag16 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag17 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag18 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag19 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag19a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag20 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag20a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag20a1 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag21 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag22 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag23 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag24 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag25 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag26 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag27 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag28 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag29 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag30 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag30a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag31 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag31a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag31a2 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag31b %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag31c %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag32 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag32a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag32a2 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag32b %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag32c %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag33 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag34 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag35 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag35a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag36 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag36a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag37 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag38 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag39 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag39a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag39b %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag40 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag40a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag41 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag41a %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag41b %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag41c %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag41d %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag41e %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag41f %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag42 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag43 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag44 %>% table(exclude = NULL)
if (verbose) bewaehr_full$bewaehrbefrag44a %>% table(exclude = NULL)
if (verbose) bewaehr_full %>% nrow
bewaehr <- bewaehr_full %>% renameCols("12. Fragebogen für Bewährungshelfende Eingangsbefragung", subset = TRUE)
if (verbose) bewaehr
if (verbose) bewaehr %>% colnames
if (verbose) bewaehr %>% head %>% as.data.frame


## join all tables to big table
## any of the IDs dplicated?
if (verbose) any(duplicated(strata$ID))
if (verbose) any(duplicated(rando$ID))
if (verbose) any(duplicated(krit$ID))
if (verbose) any(duplicated(teilnehmer$ID))
if (verbose) any(duplicated(einver$ID))
if (verbose) any(duplicated(demodat$ID))
if (verbose) any(duplicated(indexdelikt$ID))
##if (verbose) any(duplicated(indexdat$ID))
if (verbose) any(duplicated(vorstrafen$ID))
if (verbose) any(duplicated(interview$ID))
if (verbose) any(duplicated(nichtteil$ID))
if (verbose) any(duplicated(atq1$ID))
if (verbose) any(duplicated(cvtrq1$ID))
if (verbose) any(duplicated(bewaehr$ID))

## overlap of IDs between tables?
if (verbose) strata %>% nrow
if (verbose) strata %>% inner_join(rando) %>% nrow
if (verbose) strata %>% inner_join(krit) %>% nrow
if (verbose) strata %>% inner_join(teilnehmer) %>% nrow
if (verbose) strata %>% inner_join(einver) %>% nrow
if (verbose) strata %>% inner_join(demodat) %>% nrow
if (verbose) strata %>% inner_join(indexdelikt) %>% nrow
if (verbose) strata %>% inner_join(vorstrafen) %>% nrow
if (verbose) strata %>% inner_join(interview) %>% nrow
if (verbose) strata %>% inner_join(nichtteil) %>% nrow
if (verbose) strata %>% inner_join(atq1) %>% nrow
if (verbose) strata %>% inner_join(cvtrq1) %>% nrow
if (verbose) strata %>% inner_join(bewaehr) %>% nrow

client_clientlectionsdata <- readRDS(file.path(exportDirmyTabu, "client_clientlectionsdata.rds"))

client_list <- client_clientlectionsdata %>% filter(module_nr==1 & session_nr == 4 & lession_nr == 7) %>%
  .$client_id # Probanden, die mindestens 1 Modul abgeschlossen haben
client_list <- client_list[client_list!=194] # "bei Klient „myTabu016“ ist bei der Zuordnung zur Intervention ein Fehler unterlaufen. Er sollte in die Placebo-Gruppe, wurde aber der Intervention zugeordnet."
# client_list <- client_list[client_list!=329] # weibliche Klientin
client_list <- client_list[client_list!=253 & client_list!=396] # Mail vom 31.3.2025

randdat <-
  strata %>%
  full_join(rando) %>%
  full_join(krit) %>%
  full_join(teilnehmer) %>%
  full_join(einver) %>%
  full_join(demodat) %>%
  full_join(indexdelikt) %>%
  full_join(vorstrafen) %>%
  full_join(interview) %>%
  full_join(nichtteil) %>%
  full_join(atq1) %>%
  full_join(cvtrq1) %>%
  full_join(bewaehr)
if (verbose) randdat %>% nrow
if (verbose) randdat

## recruitment dates
metadat <-
  (strata_full %>% dplyr::select(mnppid:mnpfsct)) %>%
  bind_rows((krit_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((teilnehmer_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((einver_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((demodat_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((indexdelikt_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((vorstrafen_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((interview_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((nichtteil_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((atq1_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((cvtrq1_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  bind_rows((bewaehr_full %>% dplyr::select(mnppid:mnpfsct))) %>%
  dplyr::rename(ID = mnpaid)
if (verbose) metadat

recruitment <-
  metadat %>%
  group_by(ID) %>%
  dplyr::summarize(recruitmentdate = min(mnpvisstartdate)) %>%
  filter(!ID %in% c("myTabu243", "myTabu340"))# %>% #, "myTabu367" - myTabu088 hat keine client_id
  # merge(., matching, by="ID", all=TRUE) %>%
  # filter(client_id %in% client_list)
if (verbose) recruitment

recruitment <-
  recruitment %>%
  full_join(rando)
if (verbose) recruitment

if (verbose) dir(exportDirmyTabu)

infile <- file.path(exportDirmyTabu, "api_criticaleventguidedtask.rds")
guidedtask <- readRDS(infile) %>% as_tibble
if (verbose) guidedtask
## empty

infile <- file.path(exportDirmyTabu, "api_criticaleventmessage.rds")
message <- readRDS(infile) %>% as_tibble
if (verbose) message
## empty

infile <- file.path(exportDirmyTabu, "questionnaires_api_acute.rds")
acute <- readRDS(infile) %>% as_tibble
if (verbose) acute

# create new variable timepoint in order to enhance readability
# Post-Messungen = Prä-Messung des nachfolgenden Moduls
# für DSMB relevant: alle Fälle mit Module 2 (post); diese haben Modul 2
# abgeschlossen UND auch die Postmessung zu Modul 2
acute <-
  acute %>%
  mutate(timepoint = case_when(
    current_module_nr == 1 ~ 'Baseline',
    current_module_nr == 2 ~ 'Module 1 (post)',
    current_module_nr == 3 ~ 'Module 2 (post)',
    current_module_nr == 4 ~ 'Module 3 (post)',
    current_module_nr == 5 ~ 'Module 4 (post)',
    current_module_nr == 6 & current_session_nr == 1 ~ 'Module 5 (post)',
    current_module_nr == 6 & current_session_nr == 4 ~ 'Module 6 (post)'),
    timepoint = factor(timepoint))
if (verbose) acute$timepoint %>% table


infile <- file.path(exportDirmyTabu, "questionnaires_api_cmc.rds")
cmc <- readRDS(infile) %>% as_tibble
if (verbose) cmc

# create new variable timepoint in order to enhance readability
# Post-Messungen = Prä-Messung des nachfolgenden Moduls
# für DSMB relevant: alle Fälle mit Module 2 (post); diese haben Modul 2
# abgeschlossen UND auch die Postmessung zu Modul 2
cmc <-
  cmc %>%
  mutate(timepoint = case_when(
    current_module_nr == 1 ~ 'Baseline',
    current_module_nr == 2 ~ 'Module 1 (post)',
    current_module_nr == 3 ~ 'Module 2 (post)',
    current_module_nr == 4 ~ 'Module 3 (post)',
    current_module_nr == 5 ~ 'Module 4 (post)',
    current_module_nr == 6 & current_session_nr == 1 ~ 'Module 5 (post)',
    current_module_nr == 6 & current_session_nr == 4 ~ 'Module 6 (post)'),
    timepoint = factor(timepoint))
if (verbose) cmc$timepoint %>% table


infile <- file.path(exportDirmyTabu, "questionnaires_api_cte.rds")
cte <- readRDS(infile) %>% as_tibble
if (verbose) cte

# create new variable timepoint in order to enhance readability
# Post-Messungen = Prä-Messung des nachfolgenden Moduls
# für DSMB relevant: alle Fälle mit Module 2 (post); diese haben Modul 2
# abgeschlossen UND auch die Postmessung zu Modul 2
cte <-
  cte %>%
  mutate(timepoint = case_when(
    current_module_nr == 1 ~ 'Baseline',
    current_module_nr == 2 ~ 'Module 1 (post)',
    current_module_nr == 3 ~ 'Module 2 (post)',
    current_module_nr == 4 ~ 'Module 3 (post)',
    current_module_nr == 5 ~ 'Module 4 (post)',
    current_module_nr == 6 & current_session_nr == 1 ~ 'Module 5 (post)',
    current_module_nr == 6 & current_session_nr == 4 ~ 'Module 6 (post)'),
    timepoint = factor(timepoint))
if (verbose) cte$timepoint %>% table


##### WHO-5 ################

infile <- file.path(exportDirmyTabu, "questionnaires_api_who.rds")
who <- readRDS(infile) %>% as_tibble
if (verbose) who

# the who was assessed before and after each module (not only before m1 and then after the module)
#
who <-
  who %>%
  mutate(timepoint = case_when(
    current_module_nr == 1 & current_session_nr == 1 ~ 'Baseline',
    current_module_nr == 1 & current_session_nr == 4 ~ 'Module 1 (post)',
    current_module_nr == 2 & current_session_nr == 1 ~ 'Module 2 (pre)',
    current_module_nr == 2 & current_session_nr == 4 ~ 'Module 2 (post)',
    current_module_nr == 3 & current_session_nr == 1 ~ 'Module 3 (pre)',
    current_module_nr == 3 & current_session_nr == 4 ~ 'Module 3 (post)',
    current_module_nr == 4 & current_session_nr == 1 ~ 'Module 4 (pre)',
    current_module_nr == 4 & current_session_nr == 4 ~ 'Module 4 (post)',
    current_module_nr == 5 & current_session_nr == 1 ~ 'Module 5 (pre)',
    current_module_nr == 5 & current_session_nr == 4 ~ 'Module 5 (post)',
    current_module_nr == 6 & current_session_nr == 1 ~ 'Module 6 (pre)',
    current_module_nr == 6 & current_session_nr == 4 ~ 'Module 6 (post)'),
    timepoint = factor(timepoint))
if (verbose) who$timepoint %>% table

############################


infile <- file.path(exportDirmyTabu, "questionnaires_api_samplesupervisor.rds")
samplesupervisor <- readRDS(infile) %>% as_tibble
if (verbose) samplesupervisor

## create new variable timepoint in order to enhance readability
## Post-Messungen = Prä-Messung des nachfolgenden Moduls
## für DSMB relevant: alle Fälle mit Module 2 (post); diese haben Modul 2
## abgeschlossen UND auch die Postmessung zu Modul 2
samplesupervisor <-
  samplesupervisor %>%
  mutate(timepoint = case_when(
    current_module_nr == 1 ~ 'Baseline',
    current_module_nr == 2 ~ 'Module 1 (post)',
    current_module_nr == 3 ~ 'Module 2 (post)',
    current_module_nr == 4 ~ 'Module 3 (post)',
    current_module_nr == 5 ~ 'Module 4 (post)',
    current_module_nr == 6 & current_session_nr == 1 ~ 'Module 5 (post)',
    current_module_nr == 6 & current_session_nr == 4 ~ 'Module 6 (post)'),
    timepoint = factor(timepoint))
if (verbose) samplesupervisor$timepoint %>% table

## Staatsangehörigkeit
if (verbose) samplesupervisor$sample_supervisor_1 %>% table(exclude = NULL)
##     CHOICES_STAATS = [(1, 'Deutsch'), (2, 'Doppelte Staatsangehörigkeit (eine davon Deutsch)'), (3, 'Andere')]
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_1 = dplyr::recode(sample_supervisor_1,
                                      `1` = "Deutsch",
                                      `2` = "Doppelte Staatsangehörigkeit (eine davon Deutsch)",
                                      `3` = "Andere"),
         sample_supervisor_1 = factor(sample_supervisor_1,
                                      levels = c("Deutsch",
                                                 "Doppelte Staatsangehörigkeit (eine davon Deutsch)",
                                                 "Andere")))
if (verbose) samplesupervisor$sample_supervisor_1 %>% table(exclude = NULL)

if (verbose) samplesupervisor$sample_supervisor_1a %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_1a = ifelse(sample_supervisor_1 != "Deutsch", sample_supervisor_1a, as.character(sample_supervisor_1)))
if (verbose) samplesupervisor$sample_supervisor_1a %>% table(exclude = NULL)

samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(Staatsangehörigkeit = sample_supervisor_1,
                Staatsangehörigkeit2 = sample_supervisor_1a)
if (verbose) samplesupervisor %>% colnames

## Anlassdelikt
if (verbose) samplesupervisor$sample_supervisor_8 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_8 = dplyr::recode(sample_supervisor_8,
                                      `1` = '§§ 176, 176a oder 176b StGB',
                                      `2` = '§ 184b StGB',
                                      `3` = '§§ 176, 176a oder 176b StGB und § 184b StGB'))
if (verbose) samplesupervisor$sample_supervisor_8 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(Anlassdelikt = sample_supervisor_8)

## psychiatrische Diagnose
if (verbose) samplesupervisor$sample_supervisor_6 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_6 = dplyr::recode(sample_supervisor_6,
                                      `0` = 'Nein',
                                      `1` = 'Ja'))
if (verbose) samplesupervisor$sample_supervisor_6 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`psychiatrische Diagnosen (nach ICD oder DSM)` = sample_supervisor_6)

if (verbose) samplesupervisor$sample_supervisor_6a


## Dauer der angeordneten Unterstellungszeit
if (verbose) samplesupervisor$sample_supervisor_22
if (verbose) samplesupervisor$sample_supervisor_31_a
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_22 =
           ifelse(sample_supervisor_22 == -999, NA, sample_supervisor_22),
         sample_supervisor_31_a =
           ifelse(sample_supervisor_31_a == -999, NA, sample_supervisor_31_a))
if (verbose) samplesupervisor$sample_supervisor_22
if (verbose) samplesupervisor$sample_supervisor_31_a
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Dauer der Bewährungszeit (Monate)` = sample_supervisor_22,
                `Dauer der Führungsaufsicht (Monate)` = sample_supervisor_31_a)

## Dauer der bisherigen Unterstellungszeit
if (verbose) samplesupervisor$sample_supervisor_23
if (verbose) samplesupervisor$sample_supervisor_32
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_23 =
           ifelse(sample_supervisor_23 == -999, NA, sample_supervisor_23),
         sample_supervisor_32 =
           ifelse(sample_supervisor_32 == -999, NA, sample_supervisor_32))
if (verbose) samplesupervisor$sample_supervisor_23
if (verbose) samplesupervisor$sample_supervisor_32
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Bisherige Bewährungszeit (Monate)` = sample_supervisor_23,
                `Bisherige Führungsaufsicht (Monate)` = sample_supervisor_32)

## Freiheitsstrafe
if (verbose) samplesupervisor$sample_supervisor_10 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_10 = dplyr::recode(sample_supervisor_10,
                                       `0` = 'Nein',
                                       `1` = 'Ja'))
if (verbose) samplesupervisor$sample_supervisor_10 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Freiheitsstrafe` = sample_supervisor_10)
if (verbose) samplesupervisor$sample_supervisor_10a
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_10a =
           ifelse(sample_supervisor_10a == -999, NA, sample_supervisor_10a))
if (verbose) samplesupervisor$sample_supervisor_10a
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Dauer Freiheitsstrafe (Monate)` = sample_supervisor_10a)
if (verbose) samplesupervisor$sample_supervisor_10b %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_10b = dplyr::recode(sample_supervisor_10b,
                                        `1` = 'Ja',
                                        `2` = 'Ja, mit Strafrestaussetzung (§ 57 StGB)',
                                        `3` = 'Nein, die Strafe wurde zur Bewährung ausgesetzt (§ 56 StGB)',
                                        ##`4` =  'NaN',
                                        .default = NA_character_))
if (verbose) samplesupervisor$sample_supervisor_10b %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Freiheitsstrafe vollstreckt` = sample_supervisor_10b)


## Schuldfähigkeit
if (verbose) samplesupervisor$sample_supervisor_9 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_9 = dplyr::recode(sample_supervisor_9,
                                      `1` = 'Schuldunfähigkeit (§ 20 StGB)',
                                      `2` = 'verminderte Schuldfähigkeit (§ 21 StGB)',
                                      `3` = 'volle Schuldfähigkeit',
                                      ##`4` =  'NaN',
                                      .default = NA_character_))
if (verbose) samplesupervisor$sample_supervisor_9 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Schuldfähigkeit` = sample_supervisor_9)


## gerichtliche Therapieweisung
if (verbose) samplesupervisor$sample_supervisor_25 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_25 = dplyr::recode(sample_supervisor_25,
                                       `0` = 'Nein',
                                       `1` = 'Ja',
                                       .default = NA_character_))
if (verbose) samplesupervisor$sample_supervisor_25 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Weisung zur Heilbehandlung` = sample_supervisor_25)

if (verbose) samplesupervisor$sample_supervisor_35 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  mutate(sample_supervisor_35 = dplyr::recode(sample_supervisor_35,
                                       `0` = 'Nein',
                                       `1` = 'Ja',
                                       .default = NA_character_))
if (verbose) samplesupervisor$sample_supervisor_35 %>% table(exclude = NULL)
samplesupervisor <-
  samplesupervisor %>%
  dplyr::rename(`Therapieweisung` = sample_supervisor_35)

samplesupervisor <-
  samplesupervisor %>%
  mutate(`gerichtliche Therapieweisung` =
           ifelse(is.na(Therapieweisung), `Weisung zur Heilbehandlung`, Therapieweisung))
if (verbose) samplesupervisor$`gerichtliche Therapieweisung` %>% table(exclude = NULL)

#############

# ---------------
# Combine data of secutrial with data from myTabu database
#
# client_client.rds: username = Pseudonym 2 (Benutzername)
# SecuTrial: mnpaid = Pseudonym 1 (SecuTrial - ID)
# ---------------

# join with zuordnungstabelle
myTabu_Zuordnung_Pseudonyme <-
  read_excel(file.path(exportDir, "@myTabu_Zuordnung_Pseudonyme.xlsx"),
             skip = 2)
if (verbose) myTabu_Zuordnung_Pseudonyme

pseudonyms <-
  myTabu_Zuordnung_Pseudonyme %>%
  rename(mnpaid = `Pseudonym 1 (SecuTrial - ID)`,
         username = `Pseudonym 2 (Benutzername)`)
if (verbose) pseudonyms

if (verbose) dir(exportDirmyTabu)
client_client <- readRDS(file.path(exportDirmyTabu, "client_client.rds"))
if (verbose) client_client

auth_user <- readRDS(file.path(exportDirmyTabu, "auth_user.rds"))
clcl <- inner_join(client_client, auth_user, by = c("user_id" = "id")) %>% as_tibble
if (verbose) clcl

matching <-
  clcl %>%
  inner_join(pseudonyms) %>%
  dplyr::select(ID = mnpaid, client_id = id, client_group)
if (verbose) matching

#################

## acute sum_score

## wir haben alle ursprünglichen items erhoben, da zum Start der klinischen
## Studie TP 2 noch nicht ausgewertet war. Daher fließt nun nur die Hälfte der
## Items in die Auswertung mit ein

## Die Auswertung des Acute-2007-SR erfolgt durch das Aufsummieren der einzelnen
## Itemwerte. Jedes Item wird mit einem dreistufigen Antwortformat (nein,
## manchmal, oft) beurteilt. Für die Antwort nein erhält die
## Person null Punkte, für die Antwort manchmal einen Punkt und für die
## Antwort oft zwei Punkte. Eine einzige Ausnahme bildet das Item „Ist in
## den letzten Tagen eine wichtige Bezugsperson verstorben?“. In diesem Fall
## ist die dreistufige Antwortskalierung nicht sinnvoll, weshalb die Testperson
## hier zwischen den Antwortalternativen nein und ja wählen kann. In
## diesem Fall werden null (= nein) oder zwei (= ja) Punkte vergeben. Durch
## Aufsummieren der Punktwerte ergeben sich Summenwerte für die einzelnen Subskalen
## sowie ein Gesamtwert für den Acute-2007-SR. Der minimale Gesamtwert umfasst null
## Punkte, der maximale Gesamtwert umfasst 42 Punkte (21 Items; pro Item max. zwei
## Punkte). Für die einzelnen Subskalenwerte ergibt sich ebenfalls ein Minimum von
## null Punkten und ein Maximum von jeweils sechs (bzw. fünf) Punkten (drei Items
## pro Skala mit je max. zwei Punkten). Es gibt keine invertierten Items. Je höher
## der Summenwert, umso höher die Ausprägung der akuten Rückfallrisikofaktoren.
## Bei einem fehlenden Wert in einem Item kann der Mittelwert der
## anderen Items der Skala verwendet werden. Bei mehr als zwei fehlenden
## Werten bzw. bei zwei fehlenden Werten in einer Skala sollte der Test
## nicht mehr ausgewertet werden.

# -----------------
# ACUTE
# questionnaires_api_acute
# -----------------

# -999 to 0
# CHOICES_3: acute_11, acute_38
acute$acute_11_bereinigt <- ifelse(acute$acute_11 %in% c(-999, '-999'), 0, acute$acute_11)
acute$acute_38_bereinigt <- ifelse(acute$acute_38 %in% c(-999, '-999'), 0, acute$acute_38)

# ----------------
# calculate scales: classical and not reduced
# ALL ITEMS!
# ----------------

# Beschäftigung mit potentiellen Opfern
# acute_14, acute_21, acute_31, acute_42, acute_36, acute_19
#acute['acute_calc_classic_beschaeftigung'] = acute[['acute_14', 'acute_21', 'acute_31', 'acute_42', 'acute_36', 'acute_19']].sum(axis=1, skipna=False)
acute$acute_calc_classic_beschaeftigung <- rowSums(acute[, c('acute_14', 'acute_21', 'acute_31', 'acute_42', 'acute_36', 'acute_19')], na.rm = FALSE)


# Define the columns to sum for each new variable
feindseligkeit_cols <- c('acute_39', 'acute_20', 'acute_24', 'acute_8', 'acute_35', 'acute_9', 'acute_40')
hypersexualitaet_cols <- c('acute_10', 'acute_37', 'acute_41', 'acute_22', 'acute_30')
ablehnung_cols <- c('acute_3', 'acute_11_bereinigt', 'acute_34', 'acute_38_bereinigt', 'acute_7', 'acute_26', 'acute_29', 'acute_18')
emotionale_cols <- c('acute_4', 'acute_2', 'acute_43', 'acute_12', 'acute_25', 'acute_33', 'acute_13')
wegfall_cols <- c('acute_1', 'acute_5', 'acute_27', 'acute_16', 'acute_17')
substanzkonsum_cols <- c('acute_28', 'acute_6', 'acute_15', 'acute_32', 'acute_23')

# Calculate the sum of each group of columns
acute$acute_calc_classic_feindseligkeit <- rowSums(acute[, feindseligkeit_cols], na.rm = FALSE)
acute$acute_calc_classic_hypersexualitaet <- rowSums(acute[, hypersexualitaet_cols], na.rm = FALSE)
acute$acute_calc_classic_ablehnung <- rowSums(acute[, ablehnung_cols], na.rm = FALSE)
acute$acute_calc_classic_emotionale <- rowSums(acute[, emotionale_cols], na.rm = FALSE)
acute$acute_calc_classic_wegfall <- rowSums(acute[, wegfall_cols], na.rm = FALSE)
acute$acute_calc_classic_substanzkonsum <- rowSums(acute[, substanzkonsum_cols], na.rm = FALSE)

# Calculate the total score
total_cols <- c('acute_calc_classic_substanzkonsum', 'acute_calc_classic_wegfall', 'acute_calc_classic_emotionale','acute_calc_classic_ablehnung', 'acute_calc_classic_hypersexualitaet', 'acute_calc_classic_feindseligkeit', 'acute_calc_classic_beschaeftigung')
acute$acute_calc_classic_total <- rowSums(acute[, total_cols], na.rm = FALSE)

# ----------------
# calculate scales: reduced
# REDUCED ITEMS!
# ----------------

# Define the columns to sum for each new variable
beschaeftigung_cols <- c('acute_14', 'acute_21', 'acute_36')
feindseligkeit_cols <- c('acute_24', 'acute_8', 'acute_9')
hypersexualitaet_cols <- c('acute_10', 'acute_22', 'acute_30')
ablehnung_cols <- c('acute_3', 'acute_7', 'acute_18')
emotionale_cols <- c('acute_4', 'acute_33', 'acute_13')
wegfall_cols <- c('acute_5', 'acute_27', 'acute_16')
substanzkonsum_cols <- c('acute_28', 'acute_6', 'acute_15')

# Calculate the sum of each group of columns
acute$acute_calc_reduced_beschaeftigung <- rowSums(acute[, beschaeftigung_cols], na.rm = FALSE)
acute$acute_calc_reduced_feindseligkeit <- rowSums(acute[, feindseligkeit_cols], na.rm = FALSE)
acute$acute_calc_reduced_hypersexualitaet <- rowSums(acute[, hypersexualitaet_cols], na.rm = FALSE)
acute$acute_calc_reduced_ablehnung <- rowSums(acute[, ablehnung_cols], na.rm = FALSE)
acute$acute_calc_reduced_emotionale <- rowSums(acute[, emotionale_cols], na.rm = FALSE)
acute$acute_calc_reduced_wegfall <- rowSums(acute[, wegfall_cols], na.rm = FALSE)
acute$acute_calc_reduced_substanzkonsum <- rowSums(acute[, substanzkonsum_cols], na.rm = FALSE)

# Calculate the total score
total_cols <- c('acute_calc_reduced_substanzkonsum', 'acute_calc_reduced_wegfall', 'acute_calc_reduced_emotionale', 'acute_calc_reduced_ablehnung', 'acute_calc_reduced_hypersexualitaet', 'acute_calc_reduced_feindseligkeit', 'acute_calc_reduced_beschaeftigung')
acute$acute_calc_reduced_total <- rowSums(acute[, total_cols], na.rm = FALSE)

# Specify columns
columns <- c('acute_1', 'acute_2', 'acute_3', 'acute_4', 'acute_5', 'acute_6', 'acute_7',
             'acute_8', 'acute_9', 'acute_10', 'acute_12', 'acute_13', 'acute_14', 'acute_15',
             'acute_16', 'acute_18', 'acute_19', 'acute_20', 'acute_21', 'acute_22', 'acute_23',
             'acute_24', 'acute_25', 'acute_26', 'acute_28', 'acute_29', 'acute_30', 'acute_31',
             'acute_32', 'acute_33', 'acute_34', 'acute_35', 'acute_36', 'acute_37', 'acute_39',
             'acute_40', 'acute_41', 'acute_42', 'acute_43')

# Map the values for the specified columns to categories
for (column in columns) {
  acute[[column]] %<>% factor(ordered = TRUE) %>% dplyr::recode(., '0' = "Nein", '1' = "Manchmal", '2' = "Oft")
}

# For acute_17
acute[['acute_17']] %<>% factor(ordered = TRUE, levels = 2:0) %>% dplyr::recode(., '2' = "Nein", '1' = "Manchmal", '0' = "Oft")

# For acute_27
acute[['acute_27']] %<>% factor %>% recode_factor(., '0' = "Nein", '2' = "Ja")

# For acute_11 and acute_38
for (column in c('acute_11', 'acute_38')) {
  acute[[column]] %<>% factor %>% recode_factor(., '0' = "Nein", '1' = "Manchmal", '2' = "Oft", '-999' = "Ich gehe in keine therapeutische Ambulanz oder zu keinem Therapeuten (m/w/d).")
}


## cmc sum_score

## wir haben alle ursprünglichen items erhoben (n = 31), da zum Start der klinischen
## Studie TP 2 noch nicht ausgewertet war. Daher fließt nun nur die Hälfte der
## Items in die Auswertung mit ein

## Die Auswertung der CMC erfolgt ebenfalls durch das Aufsummieren der
## einzelnen Itemwerte. Jedes Item wird mit einem dreistufigen Antwortformat
## (0 x, 1 – 2 x, mehr als 2 x) beurteilt. Für die Antwort 0 x erhält die
## Person null Punkte, für die Antwort 1 – 2 x einen Punkt und für die Antwort
## mehr als 2 x zwei Punkte. Durch Aufsummieren dieser Punktwerte ergeben sich
## Summenwerte für die einzelnen Subskalen sowie ein Gesamtwert für die CMC.
## Der minimale Gesamtwert umfasst null Punkte, der maximale Gesamtwert umfasst
## 32 Punkte (16 Items; pro Item max. zwei Punkte). Für die einzelnen
## Subskalenwerte ergibt sich ebenfalls ein Minimum von null Punkten und ein
## Maximum von 16 Punkten (8 Items pro Skala mit je max. zwei Punkten). Je höher
## der Summenwert, umso öfter wurden allgemein und sexuell deviante
## Verhaltensweisen gezeigt; es gibt keine invertierten Items.


# -------------------
# CMC
# questionnaires_api_cmc
# ------------------

# Generelle Konsequenzen devianten und sexuellen Verhaltens
# cmc1, cmc2, cmc3

# in der finalen reduzierten form gibt es diese Skala nicht mehr!

# Allgemein deviantes Verhalten
cmc$cmc_calc_classic_generelle <- rowSums(cmc[c('cmc_1', 'cmc_2', 'cmc_3')], na.rm = FALSE)

cmc$cmc_calc_classic_allgemein <- rowSums(cmc[c('cmc_4', 'cmc_5', 'cmc_6', 'cmc_7', 'cmc_8', 'cmc_9', 'cmc_10',
                                                'cmc_11', 'cmc_12', 'cmc_13', 'cmc_14', 'cmc_15','cmc_16')], na.rm = FALSE)
cmc$cmc_calc_reduced_allgemein <- rowSums(cmc[c('cmc_1', 'cmc_2', 'cmc_3', 'cmc_4', 'cmc_5', 'cmc_7', 'cmc_12', 'cmc_14')], na.rm = FALSE)

# Sexuell deviantes Verhalten
cmc$cmc_calc_classic_sexuell <- rowSums(cmc[c('cmc_17', 'cmc_18', 'cmc_19', 'cmc_20', 'cmc_21', 'cmc_22', 'cmc_23',
                                              'cmc_24', 'cmc_25', 'cmc_26', 'cmc_27', 'cmc_28', 'cmc_29', 'cmc_30', 'cmc_31')], na.rm = FALSE)
cmc$cmc_calc_reduced_sexuell <- rowSums(cmc[c('cmc_17', 'cmc_20', 'cmc_21', 'cmc_22', 'cmc_23', 'cmc_24', 'cmc_25', 'cmc_26')], na.rm = FALSE)

# total score
cmc$cmc_calc_classic_total <- rowSums(cmc[c('cmc_calc_classic_generelle', 'cmc_calc_classic_allgemein',
                                            'cmc_calc_classic_sexuell')], na.rm = FALSE)

cmc$cmc_calc_reduced_total <- rowSums(cmc[c('cmc_calc_reduced_allgemein', 'cmc_calc_reduced_sexuell')], na.rm = FALSE)

columns <- c('cmc_1', 'cmc_2', 'cmc_3', 'cmc_4', 'cmc_5', 'cmc_6', 'cmc_7',
             'cmc_8', 'cmc_9', 'cmc_10', 'cmc_11', 'cmc_12', 'cmc_13',
             'cmc_14', 'cmc_15', 'cmc_16', 'cmc_17', 'cmc_18', 'cmc_19',
             'cmc_20', 'cmc_21', 'cmc_22', 'cmc_23', 'cmc_24', 'cmc_25',
             'cmc_26', 'cmc_27', 'cmc_28','cmc_29', 'cmc_30', 'cmc_31')
for (column in columns) {
  cmc[[column]] %<>% factor(ordered = TRUE) %>% dplyr::recode(., '0' = "0 Mal", '1' = "1-2 Mal", '2' = "mehr als 2 Mal")
}


## cte sum_score

## wir haben alle ursprünglichen items erhoben (n = 31), da zum Start der klinischen
## Studie TP 2 noch nicht ausgewertet war. Daher fließt nun nur die Hälfte der
## Items in die Auswertung mit ein

## Die Auswertung der CTE erfolgt ebenfalls durch das Aufsummieren der
## einzelnen Itemwerte, nachdem die 12 invertierten Items umkodiert wurden.
## Jedes Item wird mit einem dreistufigen Antwortformat (stimme zu, stimme ein
## bisschen zu, stimme nicht zu) beurteilt und es werden pro Antwort entweder
## null, ein oder zwei Punkte vergeben. Dabei ist zu beachten, dass 12 Items
## invertiert sind. Nach dem Umkodieren von invertieren Items steht eine höhere
## Punktsumme für eine höhere Ausprägung von Rückfallrisikofaktoren. Durch
## Aufsummieren dieser Punktwerte nach dem Umkodieren ergeben sich
## Summenwerte für die einzelnen Subskalen sowie ein Gesamtwert für die
## CTE. Der minimale Gesamtwert umfasst null Punkte, der maximale Gesamtwert
## umfasst 60 Punkte (30 Items; pro Item max. zwei Punkte). Für die einzelnen
## Subskalenwerte ergibt sich ebenfalls ein Minimum von null
## Punkten und ein Maximum von 10 Punkten (5 Items pro Skala mit je max. zwei
## Punkten).

## WICHTIG: invertierte Items werden bereits in der Datenbank invertiert
## gespeichert und müssen daher nicht nochmal invertiert werden!

# --------------
# STABLE-SR
# questionnaires_api_cte
# -------------

# ----------------
# calculate scales: classical and not reduced
# ALL ITEMS!
# ----------------

# Define the columns to sum for each new variable
motivation_cols <- c('cte_1', 'cte_15', 'cte_23', 'cte_55', 'cte_9', 'cte_22', 'cte_48')
motivation_reduced_cols <- c('cte_15', 'cte_55', 'cte_9', 'cte_22', 'cte_48')

sozial_cols <- c('cte_2', 'cte_16', 'cte_24', 'cte_41', 'cte_11', 'cte_33', 'cte_40', 'cte_51', 'cte_45', 'cte_10', 'cte_3', 'cte_36')
sozial_reduced_cols <- c('cte_45', 'cte_10', 'cte_3', 'cte_40', 'cte_51')

kognitionen_cols <- c('cte_39', 'cte_17', 'cte_28', 'cte_31', 'cte_37', 'cte_38', 'cte_42', 'cte_49', 'cte_20', 'cte_53', 'cte_43', 'cte_12')
kognitionen_reduced_cols <- c('cte_39', 'cte_53', 'cte_43', 'cte_49', 'cte_20')

emotionen_cols <- c('cte_25', 'cte_6', 'cte_18', 'cte_13', 'cte_4', 'cte_32', 'cte_44', 'cte_50', 'cte_58')
emotionen_reduced_cols <- c('cte_13', 'cte_4', 'cte_32', 'cte_44', 'cte_50')

problemloesungsstrategien_cols <- c('cte_26', 'cte_5', 'cte_30', 'cte_19', 'cte_46', 'cte_35', 'cte_54', 'cte_57')
problemloesungsstrategien_reduced_cols <- c('cte_26', 'cte_19', 'cte_46', 'cte_35', 'cte_57')

selbstregulation_cols <- c('cte_8', 'cte_29', 'cte_47', 'cte_14', 'cte_7', 'cte_21', 'cte_34', 'cte_52', 'cte_27', 'cte_56')
selbstregulation_reduced_cols <- c('cte_8', 'cte_56', 'cte_34', 'cte_52', 'cte_27')

# Calculate the sum of each group of columns
cte$stable_calc_classic_motivation <- rowSums(cte[, motivation_cols], na.rm = FALSE)
cte$stable_calc_reduced_motivation <- rowSums(cte[, motivation_reduced_cols], na.rm = FALSE)

cte$stable_calc_classic_sozial <- rowSums(cte[, sozial_cols], na.rm = FALSE)
cte$stable_calc_reduced_sozial <- rowSums(cte[, sozial_reduced_cols], na.rm = FALSE)

cte$stable_calc_classic_kognitionen <- rowSums(cte[, kognitionen_cols], na.rm = FALSE)
cte$stable_calc_reduced_kognitionen <- rowSums(cte[, kognitionen_reduced_cols], na.rm = FALSE)

cte$stable_calc_classic_emotionen <- rowSums(cte[, emotionen_cols], na.rm = FALSE)
cte$stable_calc_reduced_emotionen <- rowSums(cte[, emotionen_reduced_cols], na.rm = FALSE)

cte$stable_calc_classic_problemloesungsstrategien <- rowSums(cte[, problemloesungsstrategien_cols], na.rm = FALSE)
cte$stable_calc_reduced_problemloesungsstrategien <- rowSums(cte[, problemloesungsstrategien_reduced_cols], na.rm = FALSE)

cte$stable_calc_classic_selbstregulation <- rowSums(cte[, selbstregulation_cols], na.rm = FALSE)
cte$stable_calc_reduced_selbstregulation <- rowSums(cte[, selbstregulation_reduced_cols], na.rm = FALSE)

# Calculate the total score
total_cols <- c('stable_calc_classic_motivation', 'stable_calc_classic_sozial',
                'stable_calc_classic_kognitionen', 'stable_calc_classic_emotionen',
                'stable_calc_classic_problemloesungsstrategien', 'stable_calc_classic_selbstregulation')

total_reduced_cols <- c('stable_calc_reduced_motivation', 'stable_calc_reduced_sozial',
                        'stable_calc_reduced_kognitionen', 'stable_calc_reduced_emotionen',
                        'stable_calc_reduced_problemloesungsstrategien', 'stable_calc_reduced_selbstregulation')

cte$stable_calc_classic_total <- rowSums(cte[, total_cols], na.rm = FALSE)
cte$stable_calc_reduced_total <- rowSums(cte[, total_reduced_cols], na.rm = FALSE)

# Kategorisierung

columns <- c('cte_1', 'cte_2', 'cte_3', 'cte_7', 'cte_8', 'cte_10',
             'cte_13', 'cte_14', 'cte_15', 'cte_16', 'cte_18', 'cte_19',
             'cte_23', 'cte_24', 'cte_25', 'cte_29', 'cte_30', 'cte_32',
             'cte_35', 'cte_36', 'cte_40', 'cte_41', 'cte_45', 'cte_47',
             'cte_50', 'cte_57', 'cte_58')
for (column in columns) {
  cte[[column]] %<>% factor(ordered = TRUE, levels = 2:0) %>% dplyr::recode(., '0' = "Stimme zu", '1' = "Stimme ein bisschen zu", '2' = "Stimme nicht zu")
}

columns <- c('cte_4', 'cte_5', 'cte_6', 'cte_9', 'cte_11', 'cte_12',
             'cte_17', 'cte_20', 'cte_21', 'cte_22', 'cte_26', 'cte_27', 'cte_28',
             'cte_31', 'cte_33', 'cte_34', 'cte_37', 'cte_38', 'cte_39', 'cte_42',
             'cte_43', 'cte_44', 'cte_48', 'cte_49', 'cte_51', 'cte_52', 'cte_53',
             'cte_54', 'cte_55', 'cte_56')
for (column in columns) {
  cte[[column]] %<>% factor(ordered = TRUE, levels = 0:2) %>% dplyr::recode(., '2' = "Stimme zu", '1' = "Stimme ein bisschen zu", '0' = "Stimme nicht zu'")
}


## WHO-5 total score

# Calculate the total score

who$who_calc_total <- rowSums(who[, c('who_1', 'who_2', 'who_3', 'who_4', 'who_5')], na.rm = TRUE)*4


## IoD

## der IoD ergibt sich als Summe der gesamtwerte der drei Messinstrumente Acute,
## CTE und CMC. Je höher der Wert, desto höher das Rückfallrisiko.

## join

## wir ergänzen noch die subscales
acute_tmp <- acute %>% dplyr::select(client_id, timepoint, acute_calc_classic_total, acute_calc_reduced_total,
                "acute_calc_classic_beschaeftigung",
                "acute_calc_classic_feindseligkeit",
                "acute_calc_classic_hypersexualitaet",
                "acute_calc_classic_ablehnung",
                "acute_calc_classic_emotionale",
                "acute_calc_classic_wegfall",
                "acute_calc_classic_substanzkonsum",
                "acute_calc_reduced_beschaeftigung",
                "acute_calc_reduced_feindseligkeit",
                "acute_calc_reduced_hypersexualitaet",
                "acute_calc_reduced_ablehnung",
                "acute_calc_reduced_emotionale",
                "acute_calc_reduced_wegfall",
                "acute_calc_reduced_substanzkonsum")
cmc_tmp   <- cmc   %>% dplyr::select(client_id, timepoint, cmc_calc_classic_total,
    cmc_calc_classic_generelle,
    cmc_calc_classic_allgemein,
    cmc_calc_classic_sexuell
)
cte_tmp   <- cte   %>% dplyr::select(client_id, timepoint, stable_calc_classic_total, stable_calc_reduced_total,
    stable_calc_classic_motivation,
    stable_calc_reduced_motivation,
    stable_calc_classic_sozial,
    stable_calc_reduced_sozial,
    stable_calc_classic_kognitionen,
    stable_calc_reduced_kognitionen,
    stable_calc_classic_emotionen,
    stable_calc_reduced_emotionen,
    stable_calc_classic_problemloesungsstrategien,
    stable_calc_reduced_problemloesungsstrategien,
    stable_calc_classic_selbstregulation,
    stable_calc_reduced_selbstregulation
  )

### WHO-5
who_tmp   <- who %>% dplyr::select(client_id, timepoint, who_calc_total)

iod <- inner_join(acute_tmp, cmc_tmp, by = c("client_id", "timepoint"))
if (verbose) iod

iod <- iod %>% inner_join(cte_tmp, by = c("client_id", "timepoint"))
if (verbose) iod

iod <- iod %>% inner_join(who_tmp, by = c("client_id", "timepoint"))
if (verbose) iod

## calculate IoD
iod <- iod %>% mutate(IoD = stable_calc_classic_total + acute_calc_classic_total + cmc_calc_classic_total,
                      IoD_reduced = stable_calc_reduced_total + acute_calc_reduced_total)
if (verbose) iod

randdat <- merge(randdat, matching, by="ID", all=TRUE) %>%
  filter(client_id!=194)
randdat <- merge(randdat, static %>% dplyr::select(ID, static99_modified_calc), by="ID", all.x = TRUE)
dat_complete <- merge(randdat %>%
                        dplyr::select(treatment,
                               `8.1 Indexdelikt:`,
                               `8.2 Aktuelle Betreuung:`,
                               `8.3 Aktuelle zusätzliche Behandlung:`,
                               `8.4 Rückfallrisiko nach Static-99: --- Static-Score:`,
                               static99_modified_calc,
                               client_id,
                               client_group),
                      iod, by="client_id", all.x = TRUE) %>%
  mutate(client_group = recode_factor(client_group, `1`="Intervention", `2`="Placebo")) %>%
  mutate(treatment = as.factor(client_group)) %>%
  rename(`Indexdelikt` = "8.1 Indexdelikt:",
         `Aktuelle Betreuung` = "8.2 Aktuelle Betreuung:",
         `Aktuelle zusätzliche Behandlung` = "8.3 Aktuelle zusätzliche Behandlung:",
         `Baseline Static-99-Score`="8.4 Rückfallrisiko nach Static-99: --- Static-Score:") %>%
  filter(!client_id %in% c(194, 412, 511) & !is.na(client_id)) %>% # 412 kam in iod nicht vor #
  filter(client_id %in% client_list)

randdat %<>%
  mutate(client_group = recode_factor(client_group, `1`="Intervention", `2`="Placebo")) %>%
  mutate(treatment = as.factor(client_group))

############
recruitment %<>%
  filter(!ID %in% c("myTabu243", "myTabu340", "myTabu088"))# %>% #, "myTabu367" - myTabu088 hat keine client_id
Ns <-
  recruitment %>%
  filter(!is.na(treatment)) %>%
  group_by(treatment) %>%
  summarise(n = n())
if (verbose) Ns

n_aim_total <- 582
n_aim_placebo <- n_aim_total/2
n_aim_interve <- n_aim_total-n_aim_placebo

n_total   <- sum(Ns$n)
n_placebo <- Ns %>% filter(treatment == "Placebo") %>% .$n
n_interve <- Ns %>% filter(treatment == "Intervention") %>% .$n





# CVTRQ

# from Process Questionaires:
infile <- file.path(exportDirmyTabu, "questionnaires_api_cvtrq.rds")
cvtrq <- readRDS(infile) %>% as_tibble
# reverse coding
max_value <- 5  # Maximalwert der Skala
min_value <- 1  # Minimalwert der Skala

columns_to_reverse <- c('cvtrq_1', 'cvtrq_4', 'cvtrq_6', 'cvtrq_8', 'cvtrq_14',
                        'cvtrq_16', 'cvtrq_19', 'cvtrq_20')

# Für jede Spalte Reverse Coding anwenden
for (col in columns_to_reverse) {
  cvtrq <- cvtrq %>% mutate(!!paste0(col, "_reverse") := max_value + min_value - !!sym(col))
}

# Attitudes and Motivation
cvtrq <- cvtrq %>% mutate(cvtrq_calc_att = rowSums(dplyr::select(., cvtrq_1_reverse, cvtrq_2,
                                                    cvtrq_4_reverse, cvtrq_6_reverse, cvtrq_10, cvtrq_20_reverse), na.rm = FALSE))

# Emotional Reactions
cvtrq <- cvtrq %>% mutate(cvtrq_calc_emo = rowSums(dplyr::select(., cvtrq_5, cvtrq_7,
                                                    cvtrq_9, cvtrq_15, cvtrq_17, cvtrq_18), na.rm = FALSE))

# Offending Beliefs
cvtrq <- cvtrq %>% mutate(cvtrq_calc_off = rowSums(dplyr::select(., cvtrq_8_reverse,
                                                    cvtrq_12, cvtrq_14_reverse, cvtrq_16_reverse), na.rm = FALSE))

# Efficacy
cvtrq <- cvtrq %>% mutate(cvtrq_calc_eff = rowSums(dplyr::select(., cvtrq_3, cvtrq_11,
                                                    cvtrq_13, cvtrq_19_reverse), na.rm = FALSE))

# Total score
cvtrq <- cvtrq %>% mutate(cvtrq_calc_total = rowSums(dplyr::select(., cvtrq_calc_att,
                                                      cvtrq_calc_emo, cvtrq_calc_off, cvtrq_calc_eff), na.rm = FALSE))

cvtrq %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Baseline", "Module 1 (post)") %>%
                         factor(levels = levels(dat_complete$timepoint)))

infile <- file.path(exportDirmyTabu, "questionnaires_api_rcq.rds")
rcq <- readRDS(infile) %>% as_tibble

max_value <- 5  # Maximalwert der Skala
min_value <- 1  # Minimalwert der Skala

# Reverse Kodierung
columns_to_reverse <- c('rcq_1', 'rcq_10', 'rcq_12')

# Für jede Spalte Reverse Coding anwenden
for (col in columns_to_reverse) {
  rcq <- rcq %>% mutate(!!paste0(col, "_reverse") := max_value + min_value - !!sym(col))
}

# Umkodierung der Werte 1-5 in -2 bis 2
transform <- function(x, old_min, old_max, new_min, new_max) {
  return (x - old_min) * (new_max - new_min) / (old_max - old_min) + new_min
}

# Liste der Spalten, die transformiert werden sollen
columns <- c('rcq_1_reverse', 'rcq_2', 'rcq_3', 'rcq_4', 'rcq_5', 'rcq_6',
             'rcq_7', 'rcq_8', 'rcq_9', 'rcq_10_reverse', 'rcq_11', 'rcq_12_reverse')

# Transformation auf alle gewünschten Spalten anwenden
for (col in columns) {
  rcq <- rcq %>% mutate(!!paste0(col, "_recoded") := transform(!!sym(col), 1, 5, -2, 2))
}

# Scale-Scores
rcq <- rcq %>% mutate(rcq_calc_precontemplation = rowSums(dplyr::select(., rcq_1_reverse_recoded, rcq_5_recoded,
                                                               rcq_10_reverse_recoded, rcq_12_reverse_recoded), na.rm = FALSE),
                    rcq_calc_contemplation = rowSums(dplyr::select(., rcq_3_recoded, rcq_4_recoded, rcq_8_recoded, rcq_9_recoded), na.rm = FALSE),
                    rcq_calc_action = rowSums(dplyr::select(., rcq_2_recoded, rcq_6_recoded, rcq_7_recoded, rcq_11_recoded), na.rm = FALSE),
                    rcq_calc_precontemplation_reverse = rcq_calc_precontemplation * -1) %>%
  dplyr::mutate(rcq_calc_total = rowSums(dplyr::select(., rcq_calc_precontemplation, rcq_calc_contemplation, rcq_calc_action), na.rm = FALSE))


# identifiziere die Stufe des Klienten
columns <- c('rcq_calc_action', 'rcq_calc_contemplation', 'rcq_calc_precontemplation_reverse')

# Mapping von Spaltennamen zu gewünschten Werten
column_to_label <- c(
  'rcq_calc_precontemplation_reverse' = 'precontemplation',
  'rcq_calc_contemplation' = 'contemplation',
  'rcq_calc_action' = 'action'
)

# Funktion zur Bestimmung der Spalte mit maximalem Wert unter Beachtung der Priorität
max_column_with_priority <- function(row) {
  max_value <- max(row[columns])  # Maximalwert der Zeile
  for (col in columns) {  # Iteriere über Spalten in der Prioritätsreihenfolge
    if (row[col] == max_value) {
      return(column_to_label[col])  # Gib die Spalte zurück
    }
  }
}

# Neue Spalte mit den priorisierten Werten
rcq$rcq_calc_current_stage <- apply(rcq, 1, max_column_with_priority)
rcq$rcq_calc_current_stage <- factor(rcq$rcq_calc_current_stage,
                                    levels = c('precontemplation', 'contemplation', 'action'),
                                    ordered = TRUE)

rcq %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Baseline", "Module 1 (post)") %>%
                           factor(levels = levels(dat_complete$timepoint)))

# ------------
# Modul 2
# ------------

# F-SOZU: questionnaires_api_fsozu
# confirmaty with literature
infile <- file.path(exportDirmyTabu, "questionnaires_api_fsozu.rds")
fsozu <- readRDS(infile) %>% as_tibble
# Keine reverse kodierten Items in der K-7 Version!

fsozu <- fsozu %>% mutate(fsozu_calc_total = rowSums(dplyr::select(., fsozu_1, fsozu_2, fsozu_3, fsozu_4,
                                                      fsozu_5, fsozu_6, fsozu_7), na.rm = FALSE)/7)

fsozu %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 1 (post)", "Module 2 (post)") %>%
                           factor(levels = levels(dat_complete$timepoint)))

# OQMPR = ORS: questionnaires_api_ors
# confirmaty with literature
infile <- file.path(exportDirmyTabu, "questionnaires_api_ors.rds")
ors <- readRDS(infile) %>% as_tibble
# Keine reverse kodierten Items in ORS!
ors <- ors %>% mutate(ors_calc_total = rowSums(dplyr::select(., ors_1, ors_2, ors_3, ors_4,
                                                    ors_5, ors_6, ors_7, ors_8,
                                                    ors_9, ors_10, ors_11, ors_12),
                                             na.rm = FALSE))

ors %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 1 (post)", "Module 2 (post)") %>%
                           factor(levels = levels(dat_complete$timepoint)))

# UCLA: questionnaires_api_ucla
# Berechnung der Skalen/Summenwerte
infile <- file.path(exportDirmyTabu, "questionnaires_api_ucla.rds")
ucla <- readRDS(infile) %>% as_tibble
ucla <- ucla %>% mutate(
  ucla_calc_total = rowSums(dplyr::select(., ucla_1, ucla_2, ucla_3, ucla_4,
                                   ucla_5, ucla_6, ucla_7, ucla_8,
                                   ucla_9, ucla_10, ucla_11, ucla_12),
                            na.rm = FALSE),

  # Einsamkeitserleben
  ucla_calc_ein = rowSums(dplyr::select(., ucla_1, ucla_2, ucla_4, ucla_7, ucla_9),
                          na.rm = FALSE),

  # emotionale Isolation
  ucla_calc_iso = rowSums(dplyr::select(., ucla_3, ucla_8, ucla_11, ucla_12),
                          na.rm = FALSE),

  # innere Distanz
  ucla_calc_dis = rowSums(dplyr::select(., ucla_5, ucla_6, ucla_10), na.rm = FALSE)
)
ucla %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 1 (post)", "Module 2 (post)") %>%
                         factor(levels = levels(dat_complete$timepoint)))

# ------------
# Modul 3
# ------------

# BIS: questionnaires_api_bis
# confirmity with lierature
infile <- file.path(exportDirmyTabu, "questionnaires_api_bis.rds")
bis <- readRDS(infile) %>% as_tibble
max_value <- 4  # Maximalwert der Skala
min_value <- 1  # Minimalwert der Skala

columns_to_reverse <- c('bis_1', 'bis_4', 'bis_5', 'bis_7', 'bis_8', 'bis_15')

# Für jede Spalte Reverse Coding anwenden
for (col in columns_to_reverse) {
  bis <- bis %>% mutate(!!paste0(col, "_reverse") := max_value + min_value - !!sym(col))
}

# Motor impulsivity
bis <- bis %>% mutate(bis_calc_mot = rowSums(dplyr::select(., bis_10, bis_12, bis_2, bis_9, bis_13), na.rm = FALSE))

# Non-planning impulsivity
bis <- bis %>% mutate(bis_calc_non = rowSums(dplyr::select(., bis_8_reverse, bis_15_reverse, bis_5_reverse, bis_1_reverse, bis_7_reverse), na.rm = FALSE))

# Attention-based impulsivity
bis <- bis %>% mutate(bis_calc_att = rowSums(dplyr::select(., bis_14, bis_6, bis_4_reverse, bis_3, bis_11), na.rm = FALSE))

# Total score
bis <- bis %>% mutate(bis_calc_total = rowSums(dplyr::select(., bis_calc_mot, bis_calc_non, bis_calc_att), na.rm = FALSE))


bis %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 2 (post)", "Module 3 (post)") %>%
                          factor(levels = levels(dat_complete$timepoint)))

# CUSI: questionnaires_api_cusi
# exclude item 6
infile <- file.path(exportDirmyTabu, "questionnaires_api_cusi.rds")
cusi <- readRDS(infile) %>% as_tibble
# keine inverse kodierten Items in CUSI!

cusi <- cusi %>% mutate(
  cusi_calc_total = rowSums(dplyr::select(., cusi_1, cusi_2, cusi_3, cusi_4,
                                   cusi_5, cusi_6, cusi_7, cusi_8,
                                   cusi_9, cusi_10, cusi_11, cusi_12,
                                   cusi_13, cusi_14, cusi_15, cusi_16),
                            na.rm = FALSE),

  cusi_calc_con = rowSums(dplyr::select(., cusi_1, cusi_4, cusi_7, cusi_11, cusi_14),
                          na.rm = FALSE),

  cusi_calc_rap = rowSums(dplyr::select(., cusi_3, cusi_8, cusi_10, cusi_12,
                                 cusi_15, cusi_16), na.rm = FALSE),

  cusi_calc_chi = rowSums(dplyr::select(., cusi_2, cusi_5, cusi_9, cusi_13),
                          na.rm = FALSE)
)

cusi %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 2 (post)", "Module 3 (post)") %>%
                         factor(levels = levels(dat_complete$timepoint)))

# DERS: questionnaires_api_ders
# Scales/ sum values (impulse control difficulties)
infile <- file.path(exportDirmyTabu, "questionnaires_api_ders.rds")
ders <- readRDS(infile) %>% as_tibble
# keine inverse kodierten Items in DERS
# wir nutzen nur eine suskala des Fragebogens: Impulskontrollprobleme bei negativen Emotionen
ders <- ders %>% mutate(ders_calc_imp = rowSums(dplyr::select(., ders_1, ders_2,
                                                   ders_3, ders_4, ders_5), na.rm = FALSE))

ders %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 2 (post)", "Module 3 (post)") %>%
                          factor(levels = levels(dat_complete$timepoint)))

# NARQ: questionnaires_api_narq
# # Scales/ sum values
infile <- file.path(exportDirmyTabu, "questionnaires_api_narq.rds")
narq <- readRDS(infile) %>% as_tibble
max_value <- 4  # Maximalwert der Skala
min_value <- 0  # Minimalwert der Skala

columns_to_reverse <- c('narq_9', 'narq_11', 'narq_12')

# Für jede Spalte Reverse Coding anwenden
for (col in columns_to_reverse) {
  narq <- narq %>% mutate(!!paste0(col, "_reverse") := max_value + min_value - !!sym(col))
}

# Total score
narq <- narq %>% mutate(narq_calc_total = rowSums(dplyr::select(., narq_1, narq_2, narq_3, narq_4,
                                                     narq_5, narq_6, narq_7, narq_8,
                                                     narq_9_reverse, narq_10, narq_11_reverse,
                                                     narq_12_reverse, narq_13, narq_14, narq_15,
                                                     narq_16, narq_17),
                                              na.rm = FALSE))

# Neubwertung (Reappraisal)
narq <- narq %>% mutate(narq_calc_neu = rowSums(dplyr::select(., narq_1, narq_4, narq_6,
                                                   narq_7, narq_14),
                                            na.rm = FALSE))

# Unterdrückung (Suppression)
narq <- narq %>% mutate(narq_calc_unt = rowSums(dplyr::select(., narq_2, narq_9_reverse,
                                                   narq_11_reverse, narq_12_reverse, narq_16),
                                            na.rm = FALSE))

# Risikoverhalten (Externalizing Strategies)
narq <- narq %>% mutate(narq_calc_ris = rowSums(dplyr::select(., narq_3, narq_5, narq_8,
                                                   narq_10, narq_13, narq_15, narq_17),
                                            na.rm = FALSE))
narq %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 2 (post)", "Module 3 (post)") %>%
                          factor(levels = levels(dat_complete$timepoint)))

# ------------
# Modul 4
# ------------

# SPSI-R: questionnaires_api_spsi
infile <- file.path(exportDirmyTabu, "questionnaires_api_spsi.rds")
spsi <- readRDS(infile) %>% as_tibble
# # Auswahl der numerischen Spalten, die mit 'spsi_' beginnen
# numeric_columns <- grep("^spsi_", names(spsi), value = TRUE)
#
# # Konvertierung der ausgewählten Spalten in numerische Werte
# spsi[numeric_columns] <- lapply(spsi[numeric_columns], as.numeric)
#
# # Berechnung der Summe der Antworten zu den Skalenwerten pro Zeile
# spsi$spsi_raw_total <- rowSums(select(spsi, starts_with("spsi_")), na.rm = TRUE)

# Invertieren der Werte in den dysfunktionalen Skalen
invert_mapping <- c(4, 3, 2, 1, 0)

# Funktion zur Invertierung basierend auf dem Mapping
invert_function <- function(x) {
  ifelse(!is.na(x), invert_mapping[x + 1], NA)
}

# Liste der dysfunktionalen Skalen
# dysfunctional_scales <- c("spsi_1", "spsi_3", "spsi_7", "spsi_8", "spsi_11", "spsi_18", "spsi_17", "spsi_22", "spsi_10", "spsi_6", "spsi_25", "spsi_24", "spsi_14", "spsi_20", "spsi_2")
#
# # Anwendung der Invertierungsfunktion auf die ausgewählten Skalen im Datensatz
# spsi <- spsi %>%
#   mutate_at(vars(dysfunctional_scales), ~ invert_function(.))
#
# # Teilung der Skalenrohwerte durch 5
# spsi_scaled <- spsi %>%
#   mutate_at(vars(starts_with("spsi_")), ~ as.numeric(.) / 5)
#
# # Berechnung der Gesamtsumme der skalierten Werte pro Zeile
# spsi$spsi_scaled_total <- rowSums(select(spsi_scaled, starts_with("spsi_")), na.rm = TRUE)
#
# # Addition der Antworten zu den Skalenwerten
# spsi$spsi_raw_total <- rowSums(select(spsi, starts_with("spsi_")), na.rm = TRUE)
#
# # Scales/ sum values
# spsi$spsi_sum <- rowSums(select(spsi,c(spsi_1,spsi_2, spsi_3, spsi_4, spsi_5, spsi_6, spsi_7, spsi_8, spsi_9, spsi_10, spsi_11, spsi_12, spsi_13, spsi_14, spsi_15, spsi_16, spsi_17, spsi_18, spsi_19, spsi_20, spsi_21, spsi_22, spsi_23, spsi_24, spsi_25)), na.rm = TRUE)
# # subscale 1: positive problem orientation
# spsi$spsi_PosProbl <- rowSums(select(spsi,c( spsi_4, spsi_5, spsi_9, spsi_13, spsi_15)), na.rm = TRUE)
# # subscale 2: Rational problem-solving
# spsi$spsi_RatioProbl <- rowSums(select(spsi,c( spsi_12, spsi_16, spsi_19, spsi_21,spsi_23 )), na.rm = TRUE)
# # subscale 3: negative problem orientation
# spsi$spsi_NegProbl <- rowSums(select(spsi,c( spsi_1, spsi_3, spsi_7, spsi_8,spsi_11 )), na.rm = TRUE)
# # subscale 4: avoidance
# spsi$spsi_avoid <- rowSums(select(spsi,c( spsi_18, spsi_17, spsi_22, spsi_10, spsi_6  )), na.rm = TRUE)
# # subscale 5: impulsivity
# spsi$spsi_impuls <- rowSums(select(spsi,c( spsi_25, spsi_24, spsi_14, spsi_20, spsi_2)), na.rm = TRUE)
#

spsi <- spsi %>% mutate(
  # Positive Problemorientierung
  spsi_calc_ppo = rowSums(dplyr::select(., spsi_4, spsi_5, spsi_9, spsi_13, spsi_15), na.rm = FALSE) / 5,

  # Rationale Problemorientierung
  spsi_calc_rps = rowSums(dplyr::select(., spsi_12, spsi_16, spsi_19, spsi_21, spsi_23), na.rm = FALSE) / 5,

  # Negative Problemorientierung
  spsi_calc_npo = (20 - rowSums(dplyr::select(., spsi_1, spsi_3, spsi_7, spsi_8, spsi_11), na.rm = FALSE)) / 5,

  # Impulsivitäts-/ Nachlässigkeitsstil
  spsi_calc_ics = (20 - rowSums(dplyr::select(., spsi_2, spsi_14, spsi_20, spsi_24, spsi_25), na.rm = FALSE)) / 5,

  # Vermeidungsstil
  spsi_calc_as = (20 - rowSums(dplyr::select(., spsi_6, spsi_10, spsi_17, spsi_18, spsi_22), na.rm = FALSE)) / 5,

  # Total Score
  spsi_calc_total = spsi_calc_ppo + spsi_calc_rps + spsi_calc_npo + spsi_calc_ics + spsi_calc_as
)

spsi %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 3 (post)", "Module 4 (post)") %>%
                          factor(levels = levels(dat_complete$timepoint)))
#

# ------------
# Modul 5
# ------------

# BMS: questionnaires_api_kvm
# Scales/ sum values

infile <- file.path(exportDirmyTabu, "questionnaires_api_kvm.rds")
kvm <- readRDS(infile) %>% as_tibble

# keine inverse kodierten Items in BMS
kvm <- kvm %>% mutate(kvm_score = rowSums(dplyr::select(., kvm_1, kvm_2, kvm_3, kvm_4, kvm_5,
                                                   kvm_6, kvm_7, kvm_8, kvm_9, kvm_10,
                                                   kvm_11, kvm_12, kvm_13, kvm_14, kvm_15,
                                                   kvm_16, kvm_17, kvm_18, kvm_19, kvm_20,
                                                   kvm_21, kvm_22, kvm_23, kvm_24, kvm_25,
                                                   kvm_26, kvm_27, kvm_28, kvm_29, kvm_30,
                                                   kvm_31, kvm_32, kvm_33, kvm_34, kvm_35,
                                                   kvm_36, kvm_37, kvm_38),
                                            na.rm = FALSE))

kvm %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 4 (post)", "Module 5 (post)") %>%
                          factor(levels = levels(dat_complete$timepoint)))

# ------------
# Modul 6
# ------------

# EKK-R: questionnaires_api_ekk
# Recoding items

infile <- file.path(exportDirmyTabu, "questionnaires_api_ekk.rds")
ekk <- readRDS(infile) %>% as_tibble

# Reverse Coding
max_value <- 4 # Maximalwert der Skala
min_value <- 1 # Minimalwert der Skala

# Liste der Spalten für Reverse Coding
columns_to_reverse <- c('ekk_9', 'ekk_11', 'ekk_13', 'ekk_16', 'ekk_17', 'ekk_19')

# Für jede Spalte Reverse Coding anwenden
for (col in columns_to_reverse) {
  ekk[[paste0(col, '_reverse')]] <- max_value + min_value - ekk[[col]]
}

# Total Score
ekk$ekk_calc_total <- rowSums(ekk[, c('ekk_1', 'ekk_2', 'ekk_3', 'ekk_4', 'ekk_5',
                                    'ekk_6', 'ekk_7', 'ekk_8', 'ekk_9_reverse', 'ekk_10',
                                    'ekk_11_reverse', 'ekk_12', 'ekk_13_reverse', 'ekk_14', 'ekk_15',
                                    'ekk_16_reverse', 'ekk_17_reverse', 'ekk_18', 'ekk_19_reverse', 'ekk_20')], na.rm = FALSE)

# Besondere Beziehung zu Kindern
ekk$ekk_calc_bez <- rowSums(ekk[, c('ekk_1', 'ekk_2', 'ekk_3', 'ekk_4', 'ekk_5',
                                  'ekk_6', 'ekk_7', 'ekk_8', 'ekk_9_reverse')], na.rm = FALSE)

# Unreife
ekk$ekk_calc_unr <- rowSums(ekk[, c('ekk_10', 'ekk_11_reverse', 'ekk_12', 'ekk_13_reverse', 'ekk_14', 'ekk_15')], na.rm = FALSE)

# Emotionale Nähe zu Kindern
ekk$ekk_calc_emo <- rowSums(ekk[, c('ekk_16_reverse', 'ekk_17_reverse', 'ekk_18', 'ekk_19_reverse', 'ekk_20')], na.rm = FALSE)

ekk %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 5 (post)", "Module 6 (post)") %>%
                         factor(levels = levels(dat_complete$timepoint)))

# ESIQ: questionnaires_api_esiq
infile <- file.path(exportDirmyTabu, "questionnaires_api_esiq.rds")
esiq <- readRDS(infile) %>% as_tibble
# Reverse Coding
max_value <- 5 # Maximalwert der Skala
min_value <- 1 # Minimalwert der Skala

# Liste der Spalten für Reverse Coding
columns_to_reverse <- c('esiq_1', 'esiq_2', 'esiq_3', 'esiq_6', 'esiq_8',
                        'esiq_11', 'esiq_13', 'esiq_15', 'esiq_16', 'esiq_18',
                        'esiq_19', 'esiq_24', 'esiq_25', 'esiq_26', 'esiq_28',
                        'esiq_31', 'esiq_32', 'esiq_34', 'esiq_35', 'esiq_37')

# Für jede Spalte Reverse Coding anwenden
for (col in columns_to_reverse) {
  esiq[[paste0(col, '_reverse')]] <- max_value + min_value - esiq[[col]]
}

# Total Score
esiq$esiq_calc_total <- rowSums(esiq[, c('esiq_1_reverse', 'esiq_2_reverse', 'esiq_3_reverse', 'esiq_4', 'esiq_5',
                                     'esiq_6_reverse', 'esiq_7', 'esiq_8_reverse', 'esiq_9', 'esiq_10',
                                     'esiq_11_reverse', 'esiq_12', 'esiq_13_reverse', 'esiq_14', 'esiq_15_reverse',
                                     'esiq_16_reverse', 'esiq_17', 'esiq_18_reverse', 'esiq_19_reverse', 'esiq_20',
                                     'esiq_21', 'esiq_22', 'esiq_23', 'esiq_24_reverse', 'esiq_25_reverse',
                                     'esiq_26_reverse', 'esiq_27', 'esiq_28_reverse', 'esiq_29', 'esiq_30',
                                     'esiq_31_reverse', 'esiq_32_reverse', 'esiq_33', 'esiq_34_reverse', 'esiq_35_reverse',
                                     'esiq_36', 'esiq_37_reverse', 'esiq_38', 'esiq_39', 'esiq_40')], na.rm = FALSE)

# Verhalten
esiq$esiq_calc_ver <- rowSums(esiq[, c('esiq_2_reverse', 'esiq_3_reverse', 'esiq_5', 'esiq_7', 'esiq_9', 'esiq_10',
                                   'esiq_11_reverse', 'esiq_13_reverse', 'esiq_14', 'esiq_15_reverse',
                                   'esiq_16_reverse', 'esiq_18_reverse', 'esiq_20', 'esiq_24_reverse', 'esiq_25_reverse',
                                   'esiq_27', 'esiq_31_reverse',
                                   'esiq_36', 'esiq_38', 'esiq_40')], na.rm = FALSE)

# Fantasie
esiq$esiq_calc_fan <- rowSums(esiq[, c('esiq_1_reverse', 'esiq_4', 'esiq_6_reverse', 'esiq_8_reverse', 'esiq_12',
                                   'esiq_17', 'esiq_19_reverse',
                                   'esiq_21', 'esiq_22', 'esiq_23',
                                   'esiq_26_reverse', 'esiq_28_reverse', 'esiq_29', 'esiq_30',
                                   'esiq_32_reverse', 'esiq_33', 'esiq_34_reverse', 'esiq_35_reverse',
                                   'esiq_37_reverse', 'esiq_39')], na.rm = FALSE)

esiq %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 5 (post)", "Module 6 (post)") %>%
                         factor(levels = levels(dat_complete$timepoint)))

# HBI-19: questionnaires_api_hbi
infile <- file.path(exportDirmyTabu, "questionnaires_api_hbi.rds")
hbi <- readRDS(infile) %>% as_tibble
# keine invertierten Items bei HBI!
# Total Score
hbi$hbi_calc_total <- rowSums(hbi[, c('hbi_1', 'hbi_2', 'hbi_3', 'hbi_4', 'hbi_5',
                                    'hbi_6', 'hbi_7', 'hbi_8', 'hbi_9', 'hbi_10',
                                    'hbi_11', 'hbi_12', 'hbi_13', 'hbi_14', 'hbi_15',
                                    'hbi_16', 'hbi_17', 'hbi_18', 'hbi_19')], na.rm = FALSE)

# Coping
hbi$hbi_calc_cop <- rowSums(hbi[, c('hbi_1', 'hbi_3', 'hbi_6', 'hbi_8', 'hbi_13', 'hbi_16', 'hbi_18')], na.rm = FALSE)

# Konsequenzen
hbi$hbi_calc_kons <- rowSums(hbi[, c('hbi_5', 'hbi_9', 'hbi_14', 'hbi_19')], na.rm = FALSE)

# Kontrolle
hbi$hbi_calc_kont <- rowSums(hbi[, c('hbi_2', 'hbi_4', 'hbi_7', 'hbi_10', 'hbi_11', 'hbi_12', 'hbi_15', 'hbi_17')], na.rm = FALSE)

hbi %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 5 (post)", "Module 6 (post)") %>%
                          factor(levels = levels(dat_complete$timepoint)))

# SSIC: questionnaires_api_ssik
infile <- file.path(exportDirmyTabu, "questionnaires_api_ssik.rds")
ssik <- readRDS(infile) %>% as_tibble
# Reverse Coding
max_value <- 5 # Maximalwert der Skala
min_value <- 1 # Minimalwert der Skala

# Liste der Spalten für Reverse Coding
columns_to_reverse <- c('ssik_5')

# Reverse Coding anwenden
for (col in columns_to_reverse) {
  ssik[[paste0(col, '_reverse')]] <- max_value + min_value - ssik[[col]]
}

# Total Score
ssik$ssik_calc_total <- rowSums(ssik[, c('ssik_1', 'ssik_2', 'ssik_3', 'ssik_4', 'ssik_5_reverse', 'ssik_6')], na.rm = FALSE)

ssik %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 5 (post)", "Module 6 (post)") %>%
                         factor(levels = levels(dat_complete$timepoint)))


# -------
# we forgot SOI
# -------

infile <- file.path(exportDirmyTabu, "questionnaires_api_soi.rds")
soi <- readRDS(infile) %>% as_tibble

soi <- soi %>%
  dplyr::select(-soi_2) %>%
  rename(
    "soi_total_score" = soi_1
  )

soi %<>% dplyr::mutate(timepoint = if_else(current_session_nr==1, "Module 5 (post)", "Module 6 (post)") %>%
                         factor(levels = levels(dat_complete$timepoint)))


dat_baseline <- dat_complete %>%
  filter(timepoint == "Baseline") %>%
  dplyr::select(
    contains("IoD"),
    client_id,
    acute_calc_classic_total,
    acute_calc_reduced_total,
    stable_calc_classic_total,
    stable_calc_reduced_total,
    cmc_calc_classic_total,
    stable_calc_classic_motivation,
    stable_calc_reduced_motivation,
    stable_calc_classic_sozial,
    stable_calc_reduced_sozial,
    stable_calc_classic_kognitionen,
    stable_calc_reduced_kognitionen,
    stable_calc_classic_emotionen,
    stable_calc_reduced_emotionen,
    stable_calc_classic_problemloesungsstrategien,
    stable_calc_reduced_problemloesungsstrategien,
    stable_calc_classic_selbstregulation,
    stable_calc_reduced_selbstregulation,
    acute_calc_classic_beschaeftigung,
    acute_calc_classic_feindseligkeit,
    acute_calc_classic_hypersexualitaet,
    acute_calc_classic_ablehnung,
    acute_calc_classic_emotionale,
    acute_calc_classic_wegfall,
    acute_calc_classic_substanzkonsum,
    acute_calc_reduced_beschaeftigung,
    acute_calc_reduced_feindseligkeit,
    acute_calc_reduced_hypersexualitaet,
    acute_calc_reduced_ablehnung,
    acute_calc_reduced_emotionale,
    acute_calc_reduced_wegfall,
    acute_calc_reduced_substanzkonsum,
    cmc_calc_classic_allgemein,
    cmc_calc_classic_sexuell,
    who_calc_total
  ) %>%
  rename(
    `IoD baseline` = IoD,
    `IoD_reduced baseline` = IoD_reduced,
    acute_calc_classic_total_baseline = acute_calc_classic_total,
    acute_calc_reduced_total_baseline = acute_calc_reduced_total,
    stable_calc_classic_total_baseline = stable_calc_classic_total,
    stable_calc_reduced_total_baseline = stable_calc_reduced_total,
    cmc_calc_classic_total_baseline = cmc_calc_classic_total,
    stable_calc_classic_motivation_baseline = stable_calc_classic_motivation,
    stable_calc_reduced_motivation_baseline = stable_calc_reduced_motivation,
    stable_calc_classic_sozial_baseline = stable_calc_classic_sozial,
    stable_calc_reduced_sozial_baseline = stable_calc_reduced_sozial,
    stable_calc_classic_kognitionen_baseline = stable_calc_classic_kognitionen,
    stable_calc_reduced_kognitionen_baseline = stable_calc_reduced_kognitionen,
    stable_calc_classic_emotionen_baseline = stable_calc_classic_emotionen,
    stable_calc_reduced_emotionen_baseline = stable_calc_reduced_emotionen,
    stable_calc_classic_problemloesungsstrategien_baseline = stable_calc_classic_problemloesungsstrategien,
    stable_calc_reduced_problemloesungsstrategien_baseline = stable_calc_reduced_problemloesungsstrategien,
    stable_calc_classic_selbstregulation_baseline = stable_calc_classic_selbstregulation,
    stable_calc_reduced_selbstregulation_baseline = stable_calc_reduced_selbstregulation,
    acute_calc_classic_beschaeftigung_baseline = acute_calc_classic_beschaeftigung,
    acute_calc_classic_feindseligkeit_baseline = acute_calc_classic_feindseligkeit,
    acute_calc_classic_hypersexualitaet_baseline = acute_calc_classic_hypersexualitaet,
    acute_calc_classic_ablehnung_baseline = acute_calc_classic_ablehnung,
    acute_calc_classic_emotionale_baseline = acute_calc_classic_emotionale,
    acute_calc_classic_wegfall_baseline = acute_calc_classic_wegfall,
    acute_calc_classic_substanzkonsum_baseline = acute_calc_classic_substanzkonsum,
    acute_calc_reduced_beschaeftigung_baseline = acute_calc_reduced_beschaeftigung,
    acute_calc_reduced_feindseligkeit_baseline = acute_calc_reduced_feindseligkeit,
    acute_calc_reduced_hypersexualitaet_baseline = acute_calc_reduced_hypersexualitaet,
    acute_calc_reduced_ablehnung_baseline = acute_calc_reduced_ablehnung,
    acute_calc_reduced_emotionale_baseline = acute_calc_reduced_emotionale,
    acute_calc_reduced_wegfall_baseline = acute_calc_reduced_wegfall,
    acute_calc_reduced_substanzkonsum_baseline = acute_calc_reduced_substanzkonsum,
    cmc_calc_classic_allgemein_baseline = cmc_calc_classic_allgemein,
    cmc_calc_classic_sexuell_baseline = cmc_calc_classic_sexuell,
    who_calc_total_baseline = who_calc_total
  )

dat_complete <- merge(dat_complete, dat_baseline, by="client_id", all=TRUE) %>%
                merge(., cvtrq %>%
                         dplyr::select(contains("cvtrq"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                merge(., rcq %>%
                         dplyr::select(contains("rcq"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., fsozu %>%
                         dplyr::select(contains("fsozu"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., ors %>%
                         dplyr::select(contains("ors"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., ucla %>%
                         dplyr::select(contains("ucla"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., bis %>%
                         dplyr::select(contains("bis"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., cusi %>%
                         dplyr::select(contains("cusi"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., ders %>%
                         dplyr::select(contains("ders"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., narq %>%
                         dplyr::select(contains("narq"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., spsi %>%
                         dplyr::select(contains("spsi"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., kvm %>%
                         dplyr::select(contains("kvm"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., ekk %>%
                         dplyr::select(contains("ekk"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., esiq %>%
                         dplyr::select(contains("esiq"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., hbi %>%
                         dplyr::select(contains("hbi"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., ssik %>%
                         dplyr::select(contains("ssik"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                merge(., soi %>%
                         dplyr::select(contains("soi"), client_id, timepoint),
                      by=c("client_id", "timepoint"), all=TRUE) %>%
                mutate(client_id = as.factor(client_id)) %>%
                filter(!client_id %in% c(194, 412, 511) & !is.na(client_id)) %>%
                filter(client_id %in% client_list)

library(mmrm)
dat_clean <- dat_complete %>% janitor::clean_names(case="none")
# fit <- mmrm(
#   formula = IoD ~ `IoD_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` +
#     ad(timepoint | client_id),
#   data = dat_clean %>% filter(timepoint!="Baseline")
# )
# fit <- mmrm(
#   formula = IoD_reduced ~ `IoD_reduced_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` +
#     ad(timepoint | client_id), # adh(timepoint | client_id)?
#   data = dat_clean %>% filter(timepoint!="Baseline")
# )
library(lmerTest)
# fit <- lmer(IoD ~ `IoD baseline` + treatment*timepoint + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` +
#     (1 | client_id),  data = dat_complete
# )
#
# fit2 <- lmer(IoD ~ `IoD baseline` + treatment*timepoint + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` +
#               (0+timepoint | client_id),  data = dat_complete,
#              control = lmerControl(check.nobs.vs.nRE = "ignore")
# )

df_filtered <- dat_clean %>%
  group_by(client_id) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  mutate_at(c("Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung"), factor)

# dat_complete %>% filter(timepoint=="Baseline" & !is.na(treatment)) %>%
#   mutate_at(c("Indexdelikt", "Aktuelle Betreuung", "Aktuelle zusätzliche Behandlung"), factor) %>%
#   dplyr::select(`IoD baseline`, treatment, Indexdelikt, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`) %>%
#   descsuppR::buildDescrTbl(groupby = "treatment",
#                            dopvals = TRUE,
#                            addungrouped = TRUE,
#                            useutf8 = "utf8",
#                            show.IQR = TRUE,
#                            includeNAs = TRUE)

# df_wide <- dat_complete %>% filter(timepoint=="Module 2 (post)" | timepoint=="Module 3 (post)") %>%
#   tidyr::pivot_wider(names_from = timepoint, values_from = ders_sum, id_cols = "client_id") %>% merge(., dat_complete %>% filter(timepoint=="Module 2 (post)") %>% select(treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, client_id), by="client_id")
# fit <- lm(`Module 3 (post)` ~ `Module 2 (post)` +  `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung`, data = df_wide)

q_exclusion <- readRDS(file.path(exportDirmyTabu, "questionnaires_api_clientexclusionquestionnairesupervisor.rds")) %>% as_tibble
if (verbose) q_exclusion

##    YES_NO = [(1, 'Ja'), (2, 'Nein'), (3, 'NaN')]

tab_exclusions <-
  q_exclusion %>%
  dplyr::select(client_id,
                matches("^reason_[[:digit:]]$")) %>%
  tidyr::gather(reason, yesno, -client_id) %>%
  filter(yesno == 1)
if (verbose) tab_exclusions
if (verbose) tab_exclusions$client_id %>% duplicated %>% any
if (verbose) tab_exclusions$reason %>% table


## status labels
status_labels <- c('Ja', 'Nein')
status_levels = c(1,2)

q_exclusion <-
  q_exclusion %>%
  mutate_at(c(
    "reason_1",
    "reason_2",
    "reason_3",
    "reason_3b",
    "reason_3c",
    "reason_4",
    "reason_5",
    "reason_6",
    "reason_7",
    "reason_8",
    "reason_9"),
    function(x) factor(x, levels = status_levels, labels = status_labels))


## rename columns
q_exclusion <-
  q_exclusion %>%
  dplyr::select(client_id, starts_with("reason_")) %>%
  dplyr::rename(
    `Widerruf der Einverständniserklärung`                                                      = reason_1,
    `Die Zeit der Bewährungsaufsicht/Führungsaufsicht ist abgelaufen`                           = reason_2,
    `Verstoß gegen Weisungen`                                                                   = reason_3,
    `Gegen welche Weisungen wurde verstoßen (Bitte Auflistung mittels KOMMA trennen)?`          = reason_3a,
    `Kam es zu einem Strafantrag gemäß § 145a StGB?`                                            = reason_3b,
    `Kam es zu einer Verurteilung?`                                                             = reason_3c,
    `Verstoß gegen Auflagen`                                                                    = reason_4,
    `Gegen welche Auflagen wurde verstoßen (Bitte Auflistung mittels KOMMA trennen)?`           = reason_4a,
    `Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen`                       = reason_5,
    `Klient hat eine Straftat gemäß § 184b StGB begangen`                                       = reason_6,
    `Klient hat eine andere Straftat begangen (nicht gemäß §§ 176, 176a, 176b oder 184b StGB)`  = reason_7,
    `akute Selbstgefährdung des Klienten (z.B. konkrete Suizidplanung)`                         = reason_8,
    `andere Gründe als die bisher genannten Gründe`                                             = reason_9,
    `Welche Gründe (Bitte Auflistung mittels KOMMA trennen)?`                                   = reason_9a)

categorize_reason <- function(reason) {

  reason <- tolower(as.character(reason))

  if (grepl('technisch|zugang|einloggen', reason)) {
    return("Technische Probleme")
  } else if (grepl('überfordert|überforderung|überlastet|ungenügendes intellektuelles know how|kognitiv', reason)) {
    return("Kognitive Überforderung")
  } else if (grepl('emotional|psychisch|psychologische|abschließen|vergangenheit|einsam|nicht mehr aushalten', reason)) {
    return("Emotionale Überforderung")
  } else if (grepl('keine zeit|weniger zeit|zeitmangel|verpflichtungen|zeitaufwand|zeitlich|augenscheinlich die zeit', reason)) {
    return("Zeitmangel")
  } else if (grepl('therapie|behandlung|zusätzliche behandlung|anbindung an|angebunden|niedergelassen|therapeut', reason)) {
    return("Anbindung an externe f2f-Therapie")
  } else if (grepl('nicht zufrieden|nicht passende|erwartung|bedarf|gefällt nicht|nicht zurecht|bevormundet|geeignet|mehrwert|mehrwehrt', reason)) {
    return("Unzufriedenheit mit dem Programm")
  } else if (grepl('herzinfarkt|krank|krankheit', reason)) {
    return("Gesundheitsprobleme")
  } else if (grepl('gestorben|verstorben', reason)) {
    return("Tod")
  } else if (grepl('verstoß gegen interventionsvereinbarung', reason)) {
    return("Verstoß gegen Interventionsvereinbarung")
  } else if (grepl('u-haft|untersuchungshaft|hausdurchsuchung|hausdurchsuchngen|polizei|schwebendes verfahren|straftat', reason)) {
    return("Verdacht auf erneute Stratat")
  } else if (grepl('von sich aus beendet|0|klient arbeitet nicht an der studie mit, obwohl er sich dazu entschieden hatte.', reason)) {
    return("Keine Angabe von Gründen")
  } else if (grepl('internet|internetverbindung', reason)) {
    return("Internetprobleme")
  } else if (grepl('kein interesse|nicht interessiert|keine motivation|desinteresse|keine lust|interesse verloren|möchte nicht mehr|aufraffen|fehlende motivation|interesse an der studie verloren|klient arbeitet nicht an der studie mit, obwohl er sich dazu entschieden hatte', reason)) {
    return("Fehlendes Interesse oder fehlende Motivation")
  } else if (grepl('email des klienten', reason)) {
    return("Unzufriedenheit mit dem Programm")
  } else {
    return("Keine passende Kategorie")
  }
}

dat_exclusion <-
  q_exclusion %>%
  inner_join(matching) %>%
  left_join((randdat %>% dplyr::select(ID, treatment, `4.2 Bundesland:`))) %>%
  filter(!is.na(treatment)) %>%
  mutate(Category = `Welche Gründe (Bitte Auflistung mittels KOMMA trennen)?` %>% sapply(categorize_reason) %>% factor(),
         `recidivism` = if_else(`Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen`=="Ja" | `Klient hat eine Straftat gemäß § 184b StGB begangen` == "Ja" |
                                                     (`Verstoß gegen Weisungen`=="Ja" & `Gegen welche Weisungen wurde verstoßen (Bitte Auflistung mittels KOMMA trennen)?` %in% c("erneute Straftaten", "Kontakt zu Kindern unter 16 Jahren", "Kontaktaufnahme zum Kind am Sportplatz ( darf sich nicht an Plätzen wie Sportstätten etc. aufhalten)", "Sich nicht an Plätzen aufzuhalten, die üblicherweise von Kindern und Jugendlichen frequentiert werden, keinerlei Tätigkeiten auszuüben, die im Zusammenhang mit der Betreuung von Kindern und Jugendlichen stehen.") & `Kam es zu einer Verurteilung?`=="Ja"), "Ja", "Nein"),
         `evidence_for_recidivism` = if_else(`Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen`=="Ja" | `Klient hat eine Straftat gemäß § 184b StGB begangen` == "Ja" |
                                  (`Verstoß gegen Weisungen`=="Ja" & `Gegen welche Weisungen wurde verstoßen (Bitte Auflistung mittels KOMMA trennen)?` %in% c("erneute Straftaten", "Kontakt zu Kindern unter 16 Jahren", "Kontaktaufnahme zum Kind am Sportplatz ( darf sich nicht an Plätzen wie Sportstätten etc. aufhalten)", "Sich nicht an Plätzen aufzuhalten, die üblicherweise von Kindern und Jugendlichen frequentiert werden, keinerlei Tätigkeiten auszuüben, die im Zusammenhang mit der Betreuung von Kindern und Jugendlichen stehen.") & `Kam es zu einer Verurteilung?`=="Ja") | `Welche Gründe (Bitte Auflistung mittels KOMMA trennen)?` %in% c("Handy seit mehreren Monaten bei der Polizei, weitere Straftaten unklar, arbeitet nicht mit", "Aufgrund neuer Strafanzeigen in U-Haft, Gerichtsverhandlung noch ausstehend", "Anordnung Untersuchungshaft"#, "Schwebendes Verfahren (bereits vor Aufnahme) ist abgeurteilt (Straftat nach 184b)"
), "Ja", "Nein"))
# # calculate recidivism
# dat_exclusion_new <-
#   df %>%
#   mutate(
#     calc_recidivism = if_else(
#       reason_5 == "Ja" |  # "Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen"
#         reason_6 == "Ja" |  # "Klient hat eine Straftat gemäß § 184b StGB begangen"
#         (reason_3 ==" Ja" & reason_3a %in% c("erneute Straftaten", "Kontakt zu Kindern unter 16 Jahren", "Kontaktaufnahme zum Kind am Sportplatz ( darf sich nicht an Plätzen wie Sportstätten etc. aufhalten)", "Sich nicht an Plätzen aufzuhalten, die üblicherweise von Kindern und Jugendlichen frequentiert werden, keinerlei Tätigkeiten auszuüben, die im Zusammenhang mit der Betreuung von Kindern und Jugendlichen stehen.") & reason_3c == "Ja"),
#       "Ja",
#       "Nein"
#     )
#   )
#
# # calculate evdience (Hinweise/Verdacht) for recidivism
# dat_exclusion_new_2 <-
#   df %>%
#   mutate(
#     calc_evidence_for_recidivism = if_else(
#       reason_5 == "Ja" |  # "Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen"
#         reason_6 == "Ja" |  # "Klient hat eine Straftat gemäß § 184b StGB begangen"
#         (reason_3 ==" Ja" & reason_3a %in% c("erneute Straftaten", "Kontakt zu Kindern unter 16 Jahren", "Kontaktaufnahme zum Kind am Sportplatz ( darf sich nicht an Plätzen wie Sportstätten etc. aufhalten)", "Sich nicht an Plätzen aufzuhalten, die üblicherweise von Kindern und Jugendlichen frequentiert werden, keinerlei Tätigkeiten auszuüben, die im Zusammenhang mit der Betreuung von Kindern und Jugendlichen stehen.") & reason_3c == "Ja") |
#         reason_9a %in% c(
#           "Handy seit mehreren Monaten bei der Polizei, weitere Straftaten unklar, arbeitet nicht mit",
#           "Aufgrund neuer Strafanzeigen in U-Haft, Gerichtsverhandlung noch ausstehend",
#           "Anordnung Untersuchungshaft"
#           # "Schwebendes Verfahren (bereits vor Aufnahme) ist abgeurteilt (Straftat nach 184b)"
#         ),
#       "Ja",
#       "Nein"
#     )
#   )

dat_complete <- merge(dat_complete, dat_exclusion %>%
                        dplyr::select(`recidivism`, client_id),
                      all.x = TRUE) %>%
                mutate(`recidivism`=tidyr::replace_na(`recidivism`, "Nein"))

all_modules <- c("Baseline", "Module 1 (post)", "Module 2 (post)",
                 "Module 3 (post)", "Module 4 (post)", "Module 5 (post)",
                 "Module 6 (post)")
client_ids <- unique(dat_complete$client_id)
new_df <- expand.grid(client_id = client_ids, timepoint = all_modules) %>%
  dplyr::arrange(client_id)
dat_complete <- merge(new_df, dat_complete, by = c("client_id", "timepoint"), all = TRUE) %>%
  group_by(client_id) %>%
  mutate(`recidivism` = zoo::na.locf(`recidivism`),
         `IoD baseline` = zoo::na.locf(`IoD baseline`, na.rm=FALSE),
         `IoD_reduced baseline` = zoo::na.locf(`IoD_reduced baseline`, na.rm=FALSE),
         `Indexdelikt` = zoo::na.locf(`Indexdelikt`, na.rm=FALSE),
         `Aktuelle Betreuung` = zoo::na.locf(`Aktuelle Betreuung`, na.rm=FALSE),
         `Aktuelle zusätzliche Behandlung` = zoo::na.locf(`Aktuelle zusätzliche Behandlung`, na.rm=FALSE),
         `treatment` = zoo::na.locf(`treatment`, na.rm=FALSE),
         `Baseline Static-99-Score` = zoo::na.locf(`Baseline Static-99-Score`, na.rm=FALSE),
         `static99_modified_calc` = zoo::na.locf(`static99_modified_calc`, na.rm=FALSE))

replace_NA_IoD <- function(df, max_val){
  df <- df %>%
    group_by(client_id) %>%
    mutate(NOTNA=findInterval(1:n(), which(!is.na(IoD)))) %>%
    mutate(IoD=ifelse(is.na(IoD) & NOTNA==max(NOTNA) & `recidivism`=="Ja", max_val, IoD)) %>%
    dplyr::select(-NOTNA)
  return(df)
}

replace_NA_IoD_reduced <- function(df, max_val){
  df <- df %>%
    group_by(client_id) %>%
    mutate(NOTNA=findInterval(1:n(), which(!is.na(IoD_reduced)))) %>%
    mutate(IoD_reduced=ifelse(is.na(IoD_reduced) & NOTNA==max(NOTNA) & `recidivism`=="Ja", max_val, IoD_reduced)) %>%
    dplyr::select(-NOTNA)
  return(df)
}

dat_complete <- replace_NA_IoD(dat_complete, max(dat_complete$IoD, na.rm = TRUE)) # 264
dat_complete <- replace_NA_IoD_reduced(dat_complete, max(dat_complete$IoD_reduced, na.rm = TRUE))

library(mmrm)
dat_clean <- dat_complete %>% janitor::clean_names(case="none") %>%
  filter(client_id %in% client_list)

dat_clean <- dat_clean %>%
  mutate(
    Aktuelle_Betreuung = factor(
      Aktuelle_Betreuung,
      levels = c("Bewährungshilfe ohne Führungsaufsicht", "Führungsaufsicht"),
      labels = c("community supervision", "post-release supervision")
    ),
    Indexdelikt = factor(
      Indexdelikt,
      levels = c("Nur § 184b", "Auch §§ 176, 176a, 176b StGB"),
      labels = c("Hands-off (only §184b StGB)", "Hands-on (at least one conviction §176 ff StGB)")
    ),
    Aktuelle_zusatzliche_Behandlung = factor(
      Aktuelle_zusatzliche_Behandlung,
      levels = c("Nein", "Ja"),
      labels = c("No", "Yes")
    )
  )

fit1 <- mmrm(
  formula = IoD ~ `IoD_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(
      treatment = relevel(treatment, ref="Placebo")
    )
  )

fit1_acute_calc_classic <- mmrm(
  formula = acute_calc_classic_total ~ `acute_calc_classic_total_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit1_acute_calc_reduced <- mmrm(
  formula = acute_calc_reduced_total ~ `acute_calc_reduced_total_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

# subscales of acute

fit1_acute_calc_classic_beschaeftigung <- mmrm(
  formula = acute_calc_classic_beschaeftigung ~ `acute_calc_classic_beschaeftigung_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit1_acute_calc_reduced_beschaeftigung <- mmrm(
  formula = acute_calc_reduced_beschaeftigung ~ `acute_calc_reduced_beschaeftigung_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))



fit1_stable_calc_classic <- mmrm(
  formula = stable_calc_classic_total ~ `stable_calc_classic_total_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit1_stable_calc_reduced <- mmrm(
  formula = stable_calc_reduced_total ~ `stable_calc_reduced_total_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit1_cmc_calc_classic <- mmrm(
  formula = cmc_calc_classic_total ~ `cmc_calc_classic_total_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ) %>% #  & client_id 533 hat kein Modul abgeschlossen
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit1b <- mmrm(
  formula = IoD ~ `IoD_baseline` + treatment*timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396) ))

library(ggeffects)
plot1 <- ggemmeans(fit1b, terms = c("timepoint", "treatment")) %>% plot(show_data = FALSE)

fit2 <- mmrm(
  formula = IoD_reduced ~ `IoD_reduced_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id), # adh(timepoint | client_id)?
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396)) %>%
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit2b <- mmrm(
  formula = IoD_reduced ~ `IoD_reduced_baseline` + treatment*timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` +
    us(timepoint | client_id), # adh(timepoint | client_id)?
  data = dat_clean %>% filter(timepoint!="Baseline" & !client_id %in% c(253, 396))
)

plot2 <- ggemmeans(fit2b, terms = c("timepoint", "treatment")) %>% plot(show_data = FALSE)


dat_clean %<>% mutate(`delta IoD` = IoD-IoD_baseline,
                     `delta IoD_reduced` = IoD_reduced-IoD_reduced_baseline)
# dtab <-
#   dat_clean %>% ungroup() %>% dplyr::select(`delta IoD`, `delta IoD_reduced`, timepoint) %>%
#   descsuppR::buildDescrTbl(groupby="timepoint", useutf8 = "utf8", show.IQR = TRUE, includeNAs = TRUE, factorlevellimit = 45, dopvals=FALSE, descr_digits = 3, orderedAsUnordered=TRUE)
#

pretty_Pvalues <- function (p_vals, digits = 3, signiflev = 0.05, lhs = NULL, lhs_sep = "=",
                            orgbold = TRUE, roundonly = FALSE)
{
  idx_bold <- p_vals <= signiflev
  idx_too_small <- round(p_vals, digits = digits) < (1 * 10^(-digits))
  idx_bold[is.na(p_vals)] <- FALSE
  idx_too_small[is.na(p_vals)] <- FALSE
  pp_vals <- round(p_vals, digits = digits)
  pp_vals <- format(pp_vals, scientific = FALSE, digits = digits)
  pp_vals <- sapply(pp_vals, function(pval) {
    while (nchar(pval) < (digits + 2)) {
      if (length(grep(".", pval, fixed = TRUE)) > 0) {
        pval <- paste0(pval, "0")
      }
      else {
        pval <- paste0(pval, ".0")
      }
    }
    pval
  })
  names(pp_vals) <- NULL
  pp_vals[grep("NA", pp_vals)] <- NA
  if (!roundonly) {
    pp_vals[idx_too_small] <- paste0("< 0.", paste(rep("0",
                                                       digits - 1), collapse = ""), "1")
    if (!is.null(lhs)) {
      pp_vals[idx_too_small] <- paste(lhs, pp_vals[idx_too_small])
      pp_vals[!idx_too_small] <- paste(lhs, lhs_sep, pp_vals[!idx_too_small])
    }
    if (orgbold) {
      pp_vals[idx_bold] <- paste0("", pp_vals[idx_bold],
                                  "")
    }
  }
  else {
    if (!is.null(lhs)) {
      pp_vals[!idx_too_small] <- paste(lhs, lhs_sep, pp_vals[!idx_too_small])
    }
  }
  names(pp_vals) <- names(p_vals)
  return(pp_vals)
}


italic_p <- function(x, t = 0.05, q = FALSE) {
  updated_call_list <- c(x$call_list, list(italic_p = match.call()))

  # checking input table has a p.value column
  if (q == FALSE && !"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
         call. = FALSE
    )
  }

  # checking input table has a q.value column
  if (q == TRUE && !"q.value" %in% names(x$table_body)) {
    stop("There is no q-value column. `x$table_body` must have a column called 'q.value'",
         call. = FALSE
    )
  }


  # update table_styling -------------------------------------------------------
  # storing column name to bold
  col_name <- ifelse(q == FALSE, "p.value", "q.value")

  # modifying table_styling with bold threshold
  x <-
    modify_table_styling(
      x,
      columns = col_name,
      rows = !!rlang::expr(!!rlang::sym(col_name) <= !!t),
      text_format = "italic"
    )

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

library(gtsummary)
lm_Table <- fit1 %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      IoD_baseline = "Baseline value (IoD)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
#  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")

library(dplyr)
library(tidyr)

lm_Table$table_body %<>% distinct

lm_Table_acute_calc_classic <- fit1_acute_calc_classic %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      acute_calc_classic_total_baseline = "Baseline value (CARES-S)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")
lm_Table_acute_calc_classic$table_body %<>% distinct

lm_Table_acute_calc_reduced<- fit1_acute_calc_reduced %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      acute_calc_reduced_total_baseline = "Baseline value (CARES-A)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")
lm_Table_acute_calc_reduced$table_body %<>% distinct

lm_Table_stable_calc_classic <- fit1_stable_calc_classic %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      stable_calc_classic_total_baseline = "Baseline value (Stable-SR)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")
lm_Table_stable_calc_classic$table_body %<>% distinct

lm_Table_stable_calc_reduced<- fit1_stable_calc_reduced %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      stable_calc_reduced_total_baseline = "Baseline value (CARES-S)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")
lm_Table_stable_calc_reduced$table_body %<>% distinct

lm_Table2 <- fit2 %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      IoD_reduced_baseline = "Baseline value (reduced IoD)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")

lm_Table2$table_body %<>% distinct

lm_Table_cmc_calc_classic <- fit1_cmc_calc_classic %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      cmc_calc_classic_total_baseline = "Baseline value (CMC)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")
lm_Table_cmc_calc_classic$table_body %<>% distinct

library(here)
module_btimes <- readRDS(here::here("_data/MBSB/data/module_btimes.RDS"))

module_btimes_module1 <- module_btimes %>% dplyr::filter(module=="Module 1") %>% dplyr::rename(`Beginn_Modul_1` = "time_pre") %>%
  dplyr::select("Beginn_Modul_1", client_id)

df_times <- module_btimes %>%
  dplyr::mutate(recruitmentdate = recruitmentdate %>% as.POSIXct,
                `Zeit_seit_Rekrutierung_in_Jahren` = as.numeric(time_post-recruitmentdate)/365.25)
df_times %<>% base::merge(., module_btimes_module1, all.x = TRUE) %>%
  dplyr::mutate(`Zeit_seit_Beginn_Modul_1_in_Jahren` = as.numeric(time_post-`Beginn_Modul_1`)/365.25) %>%
  dplyr::rename(`timepoint` = "module") %>%
  dplyr::mutate(`timepoint` = `timepoint` %>% factor(),
                end_time = if_else(!is.na(excluded_at), excluded_at, finished_at),
                `Zeit zwischen Beginn Modul 1 und Ausschluss (Tage)` = as.numeric(end_time - as.Date(Beginn_Modul_1)))

levels(df_times$timepoint) %<>% paste(., "(post)")


dat_clean %<>% merge(., df_times %>%
                       dplyr::select(client_id,
                                     timepoint,
                                     Zeit_seit_Beginn_Modul_1_in_Jahren,
                                     Zeit_seit_Rekrutierung_in_Jahren,
                                     `Zeit zwischen Beginn Modul 1 und Ausschluss (Tage)`),
                     all.x = TRUE)

# # wir brauchen englische Begrifflichkeiten
# # translate prior placement
# ordered_labels <- c(
#   "Yes",
#   "No"
# )

# translation <- c(
#   "Ja" = "Yes",
#   "Nein" = "No"
# )

# dat_clean$Aktuelle_zusatzliche_Behandlung <- factor(translation[as.character(dat_clean$Aktuelle_zusatzliche_Behandlung)], levels = ordered_labels)

# #print(dat_clean$Aktuelle_zusatzliche_Behandlung)

# library(forcats)
# dat_clean <- dat_clean %>%
#   mutate(
#     Aktuelle_Betreuung = fct_recode(
#       Aktuelle_Betreuung,
#       "community supervision" = "Bewährungshilfe ohne Führungsaufsicht",
#       "post-release supervision" = "Führungsaufsicht"
#     ),
#     Indexdelikt = fct_recode(
#       Indexdelikt,
#       "Hands-off (only §184b StGB)" = "Nur § 184b",
#       "Hands-on (at least one conviction §176 ff StGB)" = "Auch §§ 176, 176a, 176b StGB"
#       # weitere Kategorien nach Bedarf
#     )
#   )

fit1_b <- mmrm(
  formula = IoD ~ `IoD_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` + Zeit_seit_Rekrutierung_in_Jahren +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline") %>%
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit1_c <- mmrm(
  formula = IoD ~ `IoD_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` + Zeit_seit_Beginn_Modul_1_in_Jahren +
    us(timepoint | client_id),
  data = dat_clean %>% filter(timepoint!="Baseline") %>%
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))

fit2_b <- mmrm(
  formula = IoD_reduced ~ `IoD_reduced_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` + Zeit_seit_Rekrutierung_in_Jahren +
    us(timepoint | client_id), # adh(timepoint | client_id)?
  data = dat_clean %>% filter(timepoint!="Baseline") %>%
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))


fit2_c <- mmrm(
  formula = IoD_reduced ~ `IoD_reduced_baseline` + timepoint + treatment:timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` + `static99_modified_calc` + Zeit_seit_Beginn_Modul_1_in_Jahren +
    us(timepoint | client_id), # adh(timepoint | client_id)?
  data = dat_clean %>% filter(timepoint!="Baseline") %>%
    dplyr::mutate(treatment = relevel(treatment, ref="Placebo")))


lm_Table_b <- fit1_b %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      IoD_baseline = "Baseline value (IoD)",
      Zeit_seit_Rekrutierung_in_Jahren = "Time since recruitment (years)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")

lm_Table_b$table_body %<>% distinct

lm_Table2_b <- fit2_b %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivism risk (baseline)",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      IoD_reduced_baseline = "Baseline value (reduced IoD)",
      Zeit_seit_Rekrutierung_in_Jahren = "Time since recruitment (years)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")

lm_Table2_b$table_body %<>% distinct

lm_Table_c <- fit1_c %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivsm risk",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      IoD_baseline = "Baseline value (IoD)",
      Zeit_seit_Beginn_Modul_1_in_Jahren = "Time since start module 1 (years)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )
  ) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")

lm_Table_c$table_body %<>% distinct

lm_Table2_c <- fit2_c %>%
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    intercept = TRUE,
    pvalue_fun = pretty_Pvalues,
    label = list(
      static99_modified_calc = "Static recidivsm risk",
      Aktuelle_Betreuung = "Type of supervision",
      Aktuelle_zusatzliche_Behandlung = "Additional treatment",
      Indexdelikt = "Offense type",
      IoD_reduced_baseline = "Baseline value (reduced IoD)",
      Zeit_seit_Beginn_Modul_1_in_Jahren = "Time since start module 1 (years)",
      treatment = "Treatment",
      timepoint = "Timepoint",
      `Timepoint * treatment` = "Timepoint * Treatment"
    )) %>% #, pvalue_fun = pretty_Pvalues
  #add_n(location = "level") %>% #add_nevent(location = "level") %>%
  modify_header(label ~ "**Variable**") %>%
  #  add_global_p(keep=TRUE, include = c("Gruppe")) %>%
  modify_table_styling(
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref."
  ) %>%
  modify_footnote(abbreviation = TRUE) %>%
  bold_labels() %>%
  italic_p(t = 0.05) %>%
  # CI in round brackets:
  modify_column_merge(pattern = "({conf.low}, {conf.high})",
                      rows =  conf.low!="") %>%
  modify_header(estimate ~ "**β**")

lm_Table2_c$table_body %<>% distinct

# Paired tests for IoD:
dat_complete_iod <- dat_complete %>%
  dplyr::filter(!is.na(IoD))

dat_complete_module1_iod <- dat_complete_iod %>% dplyr::filter(timepoint %in% c("Baseline", "Module 1 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Baseline" ~ "pre",
                              "Module 1 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD pre/post Module 1` = "IoD")

dat_complete_module2_iod <- dat_complete_iod %>% dplyr::filter(timepoint %in% c("Module 1 (post)", "Module 2 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 1 (post)" ~ "pre",
                              "Module 2 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD pre/post Module 2` = "IoD")
dat_complete_module3_iod <- dat_complete_iod %>% dplyr::filter(timepoint %in% c("Module 2 (post)", "Module 3 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 2 (post)" ~ "pre",
                              "Module 3 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD pre/post Module 3` = "IoD")
dat_complete_module4_iod <- dat_complete_iod %>% dplyr::filter(timepoint %in% c("Module 3 (post)", "Module 4 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 3 (post)" ~ "pre",
                              "Module 4 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD pre/post Module 4` = "IoD")
dat_complete_module5_iod <- dat_complete_iod %>% dplyr::filter(timepoint %in% c("Module 4 (post)", "Module 5 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 4 (post)" ~ "pre",
                              "Module 5 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD pre/post Module 5` = "IoD")
dat_complete_module6_iod <- dat_complete_iod %>% dplyr::filter(timepoint %in% c("Module 5 (post)", "Module 6 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 5 (post)" ~ "pre",
                              "Module 6 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD pre/post Module 6` = "IoD")

dat_complete_iod <- dat_complete_module1_iod %>%
  dplyr::select(contains("pre/post"), client_id, timepoint, treatment) %>%
  dplyr::filter(!client_id %in% c(253, 396)) %>%
  merge(.,
        dat_complete_module2_iod %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module3_iod %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module4_iod %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module5_iod %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module6_iod %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  arrange(client_id, desc(timepoint)) %>%
  dplyr::filter(!is.na(timepoint)) %>%
  dplyr::mutate(timepoint = relevel(timepoint, "pre"))

w.nparcomp.paired <- function(values, grouping, ...) {
  values <- as.numeric(values)
  splitGrps <- function(values, grouping) {
    glevs <- unique(grouping)

    res <-
      lapply(glevs, function(glev)
        values[grouping == glev])

    names(res) <- glevs

    res
  }
  gvalues <- splitGrps(values, grouping)
  if (length(gvalues) != 2) stop("w.nparcomp.test is applicable with *two* groups only")
  df_tmp <- data.frame(gr=grouping, val=values)
  npar.t.test.paired_wrapper <- function(...) {
    res <- nparcomp::npar.t.test.paired(...)
    res$methodvec  <- "Brunner-Munzel test for paired data"
    res$p.value <- res$Analysis[1,5] # res$Analysis[2,5] <-> PERM, res$Analysis[1,5] <-> Brunner-Munzel
    # at least 15 subjects for Brunner-Munzel!
    return(res)
  }
  set.seed(123)
  plyr::failwith(default = list(statistic = NA,
                                parameter = NULL,
                                p.value = NA,
                                null.value = NULL,
                                alternative = NULL,
                                method = NULL,
                                data.name = NULL, paired=TRUE),
                 f = npar.t.test.paired_wrapper)(val ~ gr,
                                                 data = df_tmp, alternative = "two.sided", info=FALSE, plot.simci=FALSE, nperm=10000,
                                                 ...)
}

tests <- c(`IoD pre/post Module 1` = "w.nparcomp.paired",
           `IoD pre/post Module 2` = "w.nparcomp.paired",
           `IoD pre/post Module 3` = "w.nparcomp.paired",
           `IoD pre/post Module 4` = "w.nparcomp.paired",
           `IoD pre/post Module 5` = "w.nparcomp.paired",
           `IoD pre/post Module 6` = "w.nparcomp.paired")

dat_complete_intervention_iod <- dat_complete_iod %>%
  dplyr::filter(treatment=="Intervention")
dat_complete_placebo_iod <- dat_complete_iod %>%
  dplyr::filter(treatment=="Placebo")

# dtab_intervention_iod <- dat_complete_intervention_iod %>%
#   dplyr::select(contains("pre/post"), timepoint) %>% descsuppR::buildDescrTbl(
#   groupby = "timepoint",
#   useutf8 = "utf8",
#   show.IQR = TRUE,
#   includeNAs = TRUE,
#   factorlevellimit = 45,
#   dopvals = TRUE,
#   tests = tests,
#   descr_digits = 3,
#   pvals_digits = 3,
#   orderedAsUnordered = TRUE,
#   report_tests = TRUE,
#   report_testmessages = FALSE)

# dtab_placebo_iod <- dat_complete_placebo_iod %>%
#   dplyr::select(contains("pre/post"), timepoint) %>% descsuppR::buildDescrTbl(
#     groupby = "timepoint",
#     useutf8 = "utf8",
#     show.IQR = TRUE,
#     includeNAs = TRUE,
#     factorlevellimit = 45,
#     dopvals = TRUE,
#     tests = tests,
#     descr_digits = 3,
#     pvals_digits = 3,
#     orderedAsUnordered = TRUE,
#     report_tests = TRUE,
#     report_testmessages = FALSE)

# Reshape the data: separate "pre" and "post" values into columns
df_wide_iod <- dat_complete_iod %>%
  dplyr::select(contains("pre/post"), timepoint, client_id, treatment) %>%
  tidyr::pivot_wider(names_from = timepoint, values_from = -c(client_id, timepoint, treatment)) %>%
  dplyr::mutate(`treatment` = `treatment` %>% relevel(ref = "Placebo"))
names(df_wide_iod) %<>% gsub("pre/post ", "", .)
# Compute the differences (post - pre)
df_diff_iod <- df_wide_iod %>%
  mutate(across(ends_with("post"), ~ . - get(sub("post", "pre", cur_column())), .names = "diff_{.col}")) %>%
  rename_with(~ sub("_post$", "", .), starts_with("diff_")) %>%
  dplyr::select(client_id, starts_with("diff_"), treatment)

generate_named_vector <- function(variable_names, value) {
  # Create a named vector with the given value for each variable
  named_vector <- setNames(rep(value, length(variable_names)), variable_names)
  return(named_vector)
}
variable_names <- df_diff_iod %>% select(contains("diff")) %>% names
tests <- generate_named_vector(variable_names, "w.npar.t.test")

# dtab_iod <- df_diff_iod %>% dplyr::select(-client_id) %>%
#   filter(!is.na(treatment)) %>%
#   descsuppR::buildDescrTbl(groupby = "treatment",
#                            useutf8 = "utf8",
#                            show.IQR = TRUE,
#                            includeNAs = TRUE,
#                            factorlevellimit = 45,
#                            dopvals = TRUE,
#                            tests = tests,
#                            descr_digits = 3,
#                            pvals_digits = 3,
#                            orderedAsUnordered = TRUE,
#                            report_tests = TRUE,
#                            report_testmessages = FALSE) # 332 kommt hier nicht vor, da post Modul1-Wert fehlt.

# dplot_iod <- dtab_iod %>%
#   descsuppRplots::plotDescTbl(centrality.plotting = FALSE)

# Paired tests for the reduced IoD:
dat_complete_iod_reduced <- dat_complete %>%
  dplyr::filter(!is.na(IoD_reduced))

dat_complete_module1_iod_reduced <- dat_complete_iod_reduced %>% dplyr::filter(timepoint %in% c("Baseline", "Module 1 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Baseline" ~ "pre",
                              "Module 1 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD_reduced pre/post Module 1` = "IoD_reduced")

dat_complete_module2_iod_reduced <- dat_complete_iod_reduced %>% dplyr::filter(timepoint %in% c("Module 1 (post)", "Module 2 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 1 (post)" ~ "pre",
                              "Module 2 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD_reduced pre/post Module 2` = "IoD_reduced")
dat_complete_module3_iod_reduced <- dat_complete_iod_reduced %>% dplyr::filter(timepoint %in% c("Module 2 (post)", "Module 3 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 2 (post)" ~ "pre",
                              "Module 3 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD_reduced pre/post Module 3` = "IoD_reduced")
dat_complete_module4_iod_reduced <- dat_complete_iod_reduced %>% dplyr::filter(timepoint %in% c("Module 3 (post)", "Module 4 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 3 (post)" ~ "pre",
                              "Module 4 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD_reduced pre/post Module 4` = "IoD_reduced")
dat_complete_module5_iod_reduced <- dat_complete_iod_reduced %>% dplyr::filter(timepoint %in% c("Module 4 (post)", "Module 5 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 4 (post)" ~ "pre",
                              "Module 5 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD_reduced pre/post Module 5` = "IoD_reduced")
dat_complete_module6_iod_reduced <- dat_complete_iod_reduced %>% dplyr::filter(timepoint %in% c("Module 5 (post)", "Module 6 (post)"))  %>%
  group_by(client_id) %>%
  filter(n()==2) %>%
  mutate(timepoint=case_match(timepoint,
                              "Module 5 (post)" ~ "pre",
                              "Module 6 (post)" ~ "post") %>%
           factor()) %>%
  dplyr::rename(`IoD_reduced pre/post Module 6` = "IoD_reduced")

dat_complete_iod_reduced <- dat_complete_module1_iod_reduced %>%
  dplyr::select(contains("pre/post"), client_id, timepoint, treatment) %>%
  dplyr::filter(!client_id %in% c(253, 396)) %>%
  merge(.,
        dat_complete_module2_iod_reduced %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module3_iod_reduced %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module4_iod_reduced %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module5_iod_reduced %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  merge(.,
        dat_complete_module6_iod_reduced %>%
          dplyr::select(contains("pre/post"), client_id, timepoint) %>%
          dplyr::filter(!client_id %in% c(253, 396)),
        by=c("client_id", "timepoint"),
        all=TRUE) %>%
  arrange(client_id, desc(timepoint)) %>%
  dplyr::filter(!is.na(timepoint)) %>%
  dplyr::mutate(timepoint = relevel(timepoint, "pre"))

w.nparcomp.paired <- function(values, grouping, ...) {
  values <- as.numeric(values)
  splitGrps <- function(values, grouping) {
    glevs <- unique(grouping)

    res <-
      lapply(glevs, function(glev)
        values[grouping == glev])

    names(res) <- glevs

    res
  }
  gvalues <- splitGrps(values, grouping)
  if (length(gvalues) != 2) stop("w.nparcomp.test is applicable with *two* groups only")
  df_tmp <- data.frame(gr=grouping, val=values)
  npar.t.test.paired_wrapper <- function(...) {
    res <- nparcomp::npar.t.test.paired(...)
    res$methodvec  <- "Brunner-Munzel test for paired data"
    res$p.value <- res$Analysis[1,5] # res$Analysis[2,5] <-> PERM, res$Analysis[1,5] <-> Brunner-Munzel
    # at least 15 subjects for Brunner-Munzel!
    return(res)
  }
  set.seed(123)
  plyr::failwith(default = list(statistic = NA,
                                parameter = NULL,
                                p.value = NA,
                                null.value = NULL,
                                alternative = NULL,
                                method = NULL,
                                data.name = NULL, paired=TRUE),
                 f = npar.t.test.paired_wrapper)(val ~ gr,
                                                 data = df_tmp, alternative = "two.sided", info=FALSE, plot.simci=FALSE, nperm=10000,
                                                 ...)
}

tests <- c(`IoD_reduced pre/post Module 1` = "w.nparcomp.paired",
           `IoD_reduced pre/post Module 2` = "w.nparcomp.paired",
           `IoD_reduced pre/post Module 3` = "w.nparcomp.paired",
           `IoD_reduced pre/post Module 4` = "w.nparcomp.paired",
           `IoD_reduced pre/post Module 5` = "w.nparcomp.paired",
           `IoD_reduced pre/post Module 6` = "w.nparcomp.paired")

dat_complete_intervention_iod_reduced <- dat_complete_iod_reduced %>%
  dplyr::filter(treatment=="Intervention")
dat_complete_placebo_iod_reduced <- dat_complete_iod_reduced %>%
  dplyr::filter(treatment=="Placebo")

# dtab_intervention_iod_reduced <- dat_complete_intervention_iod_reduced %>%
#   dplyr::select(contains("pre/post"), timepoint) %>% descsuppR::buildDescrTbl(groupby = "timepoint",
#                                                                               useutf8 = "utf8",
#                                                                               show.IQR = TRUE,
#                                                                               includeNAs = TRUE,
#                                                                               factorlevellimit = 45,
#                                                                               dopvals = TRUE,
#                                                                               tests = tests,
#                                                                               descr_digits = 3,
#                                                                               pvals_digits = 3,
#                                                                               orderedAsUnordered = TRUE,
#                                                                               report_tests = TRUE,
#                                                                               report_testmessages = FALSE)

# dtab_placebo_iod_reduced <- dat_complete_placebo_iod_reduced %>%
#   dplyr::select(contains("pre/post"), timepoint) %>% descsuppR::buildDescrTbl(groupby = "timepoint",
#                                                                               useutf8 = "utf8",
#                                                                               show.IQR = TRUE,
#                                                                               includeNAs = TRUE,
#                                                                               factorlevellimit = 45,
#                                                                               dopvals = TRUE,
#                                                                               tests = tests,
#                                                                               descr_digits = 3,
#                                                                               pvals_digits = 3,
#                                                                               orderedAsUnordered = TRUE,
#                                                                               report_tests = TRUE,
#                                                                               report_testmessages = FALSE)

# Reshape the data: separate "pre" and "post" values into columns
df_wide_iod_reduced <- dat_complete_iod_reduced %>%
  dplyr::select(contains("pre/post"), timepoint, client_id, treatment) %>%
  tidyr::pivot_wider(names_from = timepoint, values_from = -c(client_id, timepoint, treatment)) %>%
  dplyr::mutate(`treatment` = `treatment` %>% relevel(ref = "Placebo"))
names(df_wide_iod_reduced) %<>% gsub("pre/post ", "", .)

# Compute the differences (post - pre)
df_diff_iod_reduced <- df_wide_iod_reduced %>%
  mutate(across(ends_with("post"), ~ . - get(sub("post", "pre", cur_column())), .names = "diff_{.col}")) %>%
  rename_with(~ sub("_post$", "", .), starts_with("diff_")) %>%
  dplyr::select(client_id, starts_with("diff_"), treatment)

generate_named_vector <- function(variable_names, value) {
  # Create a named vector with the given value for each variable
  named_vector <- setNames(rep(value, length(variable_names)), variable_names)
  return(named_vector)
}
variable_names <- df_diff_iod_reduced %>% select(contains("diff")) %>% names
tests <- generate_named_vector(variable_names, "w.npar.t.test")

# dtab_iod_reduced <- df_diff_iod_reduced %>% dplyr::select(-client_id) %>%
#   filter(!is.na(treatment)) %>%
#   descsuppR::buildDescrTbl(groupby = "treatment",
#                            useutf8 = "utf8",
#                            show.IQR = TRUE,
#                            includeNAs = TRUE,
#                            factorlevellimit = 45,
#                            dopvals = TRUE,
#                            tests = tests,
#                            descr_digits = 3,
#                            pvals_digits = 3,
#                            orderedAsUnordered = TRUE,
#                            report_tests = TRUE,
#                            report_testmessages = FALSE)

# dplot_iod_reduced <- dtab_iod_reduced %>%
#   descsuppRplots::plotDescTbl(centrality.plotting = FALSE)

#### Kaplan-Meier

library(bpcp)
dat_clean_first <- dat_clean %>%
  filter(timepoint!="Baseline") %>%
  group_by(client_id) %>%
  summarise_all(first) %>%
  ungroup()
sfit <- bpcpfit(Surv(`Zeit zwischen Beginn Modul 1 und Ausschluss (Tage)`/365.25, recidivism) ~ treatment, data = dat_clean_first %>%
                  mutate(recidivism = if_else(recidivism=="Ja", 1, 0)))
sfit.ci <- tidykmciLR(sfit)
p_km_itt <-
  sfit.ci %>%
  mutate(group = factor(group, levels = levels(dat_clean_first$treatment))) %>%
  ggplot(aes(x = time, y = surv, ymin = lower, ymax = upper, col = group)) +
  ##geom_vline(aes(xintercept = day), data = lvisitplan) +
  ## geom_ribbon(aes(xmin = xmin, xmax = xmax, y = y, group = VisitId), inherit.aes = FALSE,
  ##             data = (lvisitplan %>%
  ##                     mutate(ymin = -Inf, ymax = Inf) %>%
  ##                     tidyr::gather(what, y, ymin, ymax)),
  ##             alpha = 0.2) +
  geom_ribbon(alpha = .2, aes(fill=group), colour = NA) +
  geom_line(aes(lty = group)) +
  geom_point(aes(x = `Zeit zwischen Beginn Modul 1 und Ausschluss (Tage)`, y = 1, col = group),
             inherit.aes = FALSE,
             pch = "|",
             size = 3,
             data = (dat_clean_first  %>% filter(recidivism == 0) %>% dplyr::rename(group = treatment))) +
  # scale_x_continuous(breaks = as.numeric(lvisitplan$day %>% gsub(171, 180, .)),
  #                    ##labels = glue("{lvisitplan$day}\n({lvisitplan$VisitId})"),
  #                    labels = glue("{lvisitplan$day %>% gsub(minV23, 180, .)}"),
  #                    ##limits = c(0, max(lvisitplan$xmax)),
  #                    expand = c(0.05, 0, 0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y = 0:1) +
  # scale_colour_manual(values = clrs$treatment) +
  # scale_fill_manual(values = clrs$treatment) +
  xlab("Time [years] since beginning of module 1") +
  ylab("Safety event free probability") +
  ggtitle("Kaplan Meier Curves for Time to recidivism") +
  theme_bw()

sfit2 <- survfit(Surv(`Zeit zwischen Beginn Modul 1 und Ausschluss (Tage)`/365.25, recidivism) ~ treatment, data = dat_clean_first %>%
                   mutate(recidivism = if_else(recidivism=="Ja", 1, 0)))
p_risktab_itt <-
  survminer::ggsurvtable(sfit2, data = (dat_clean_first %>% mutate(treatment = factor(treatment, levels = levels(treatment)))),
                         survtable = "risk.table",
                         risk.table.type = "absolute",
                         color = "treatment",
                         ##xlim = c(0, max(lvisitplan$xmax)),
                         break.time.by = 0.5) +
  scale_x_continuous(breaks = 0:2) +
  # scale_x_continuous(breaks = lvisitplan$day,
  #                    labels = glue("{lvisitplan$VisitId}"),
  #                    ##labels = glue("{lvisitplan$day}\n({lvisitplan$VisitId})"),
  #                    ##limits = c(0, max(lvisitplan$xmax)),
  #                    expand = c(0.01, 0, 0, 0)) +
  # ##scale_colour_manual(values = scales::hue_pal()(3)[c(2,3,1)]) + ## hack
  # scale_colour_manual(values = clrs$Arm) +
  theme_bw() +
  theme(legend.position = "none")

library(patchwork)
p_km_risktab_itt <-
  p_km_itt +
  (p_risktab_itt +
     labs(x = "Time [years] since beginning of module 1",
          y= NULL) +
     theme(panel.grid = element_blank())) +
  plot_layout(ncol = 1, heights = c(0.8, 0.2))

kmciLRextract <- function(fit, time)
{
  fit_df <-
    fit %>%
    summary %>%
    mutate(time.lower = gsub(",.*", "", `time interval`),
           time.lower = gsub("^\\[", "", time.lower),
           time.lower = as.numeric(time.lower)) %>%
    mutate(time.upper = gsub(".*,", "", `time interval`),
           time.upper = gsub(")$", "", time.upper),
           time.upper = as.numeric(time.upper)) %>%
    as_tibble
  fit_df %>% filter(time.lower <= time, time.upper > time)
}

proportions_noevent_safety_itt <-
  sfit %>%
  kmciLRextract(time = 1) %>%
  mutate(ci = descutils::prettyCI(`lower 95% CL`, `upper 95% CL`, digits = 2))

## from https://stackoverflow.com/a/31198761
mytidy_survfitsummary <- function(sfit, time)
{
  sfit_summary <-
    sfit %>%
    summary(times = time)
  cols <- lapply(c(2:6, 8:11) , function(x) sfit_summary[x])
  tbl <- do.call(data.frame, cols) %>% as_tibble
  tbl
}
survfitExtractN <- function(fit, time)
{
  num  <- fit %>% mytidy_survfitsummary(time)
  num0 <- fit %>% mytidy_survfitsummary(0)

  num0 %>% dplyr::select(strata, n = n.risk) %>% full_join((num %>% dplyr::select(strata, n.event, n.censor)))
}
Ns <- sfit2 %>% survfitExtractN(1) %>%
  dplyr::rename(treatment = strata) %>%
  mutate(treatment = gsub("^treatment=", "", treatment))

proportions_noevent_safety_itt <-
  proportions_noevent_safety_itt %>%
  dplyr::select(treatment, proportion = survival, `95% CI` = ci) %>%
  inner_join(Ns) %>%
  dplyr::select(treatment, n, n.event, n.censor, everything())

cidelta_safety_pp <-
  lapply(c("Intervention"), function(dose)
  {
    prop.diff <-
      dat_clean_first %>%
      mutate(recidivism = if_else(recidivism=="Ja", 1, 0)) %>%
      {
        bpcp2samp(.$`Zeit zwischen Beginn Modul 1 und Ausschluss (Tage)`/365.25, .$recidivism, .$treatment, testtime = 1,
                  parmtype = "difference")
      }
    prop.diff %>%
      tidy %>%
      mutate(treatment = dose) %>%
      mutate(comparison = prop.diff$data.name) %>%
      mutate(conf.level = attr(prop.diff$conf.int, "conf.level")) %>%
      dplyr::rename(!!sym(attr(prop.diff$estimate, "names")) := estimate)
  }) %>%
  bind_rows %>%
  dplyr::select(comparison, difference, conf.low, conf.high, conf.level, p.value) %>%
  mutate(ci = descutils::prettyCI(conf.low, conf.high, digits = 2)) %>%
  mutate(`P value` = descutils::prettyPvalues(p.value, digits = 2)) %>%
  dplyr::rename(!!sym(glue::glue("{.$conf.level[[1]]*100}%-CI")) := ci) %>%
  dplyr::select(-conf.low, -conf.high, -conf.level, -p.value) %>%
  mutate(comparison = gsub("\\)-S\\(", ") - S(", comparison)) %>%
  mutate(comparison = gsub("S\\([[:digit:]]+;group=([^)]+)\\)", "\\1", comparison))


# CONSORT diagram
# dat_exclusion %>% filter(!client_id %in% module_btimes$client_id) # 61
# q_exclusion$client_id %>% unique() %>% length()
# dat_exclusion$client_id %>% unique() %>% length()
# module_btimes$client_id %>% unique() %>% length() # Modul 1 begonnen: 303
# dat_complete$client_id  %>% unique() %>% length()
# recruitment %>%
#   filter(!is.na(treatment)) %>% nrow() # 369 wurden rekrutiert, 61 davon haben M1 nicht begonnen (s.o.) und sind in dat_exclusion enthalten. -> übrigen 6 untersuchen
