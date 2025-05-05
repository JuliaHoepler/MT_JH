library(haven)
library(data.table)
library(dplyr)

source("functions.R")

ist_data <- read_sas("IST-3 Data/Raw Data/datashare_aug2015.sas7bdat")
ist_data$outcome <- ist_data$aliveind6 -1
dat <- data.table(ist_data)

data <- as.data.frame(dat)



################################################################################
####################### Apply function to transform data #######################
################################################################################

data_transformed <- transform_variables(df = data)

check <- variable_summary(df = data, transformed_df = data_transformed)




################################################################################
###### After checking all variables manually (see the check data frame), #######
###### the following variables have to be adapted                        #######
################################################################################


# Exclude manually transformed variables from the automatic transformation process
excluded_vars <- c(
  "randhosp_id", "randpat_id", "randyear", "randmonth", "randhour", "randmin",
  "livealone_rand", "infarct", "gcs_eye_rand", "gcs_motor_rand", "gcs_verbal_rand", 
  "gcs_score_rand", "stroketype", "anticoag_pre", "asl", "aspirin_pre", "dipyridamole_pre", 
  "clopidogrel_pre", "lowdose_heparin_pre", "fulldose_heparin_pre", "warfarin_pre", 
  "hypertension_pre", "diabetes_pre", "stroke_pre", "aspirin_day1", "antiplatelet_day1", 
  "lowdose_heparin_day1", "full_anticoag_day1", "lowerBP_day1", "nontrial_thromb_day1", 
  "iv_fluids_day1", "insulin_day1", "aspirin_days2to7", "antiplatelet_days2to7", 
  "lowdose_heparin_days2to7", "full_anticoag_days2to7", "lowerBP_days2to7", 
  "nontrial_thromb_days2to7", "nasogastric_days2to7", "antibiotics_days2to7", 
  "findiag7", "nonstroke_type7", "med_adno", "critcareno", "strk_unitno", 
  "genwardno", "sevendaycase", "gcs_eye_7", "gcs_motor_7", "gcs_verbal_7", 
  "liftarms_7", "ablewalk_7", "indepinadl_7", "destination", "ohs6", "ordinal6", 
  "event_days", "nslesion2", "nslesion3", "nslesion4", "time_to_PH", "haltcode", 
  "recinfus","recR","recP","rec7","adjudicated","sich7","dead7","recsix",
  "dead6mo","deadordep18", "R_mca_aspects", "R_tot_aspects"
)


data_transformed_2 <- transform_variables_2(df = data, excluded_vars = excluded_vars)
check_2 <- variable_summary(df = data, transformed_df = data_transformed_2)



################################################################################
############### Now transform the excluded variables manually ##################
################################################################################




library(dplyr)

data_transformed_3 <- data_transformed_2 %>%
  mutate(
    
    # Numeric -> Leave as Numeric
    randyear = as.numeric(randyear),
    randmonth = as.numeric(randmonth),
    randhour = as.numeric(randhour),
    randmin = as.numeric(randmin),
    
    # Variables with format YN
    pretrialexp = factor(pretrialexp, levels = c(1, 2), labels = c('Yes', 'No')),
    aliveind6 = factor(aliveind6, levels = c(1, 2), labels = c('Yes', 'No')),
    aliveind18 = factor(aliveind18, levels = c(1, 2), labels = c('Yes', 'No')),
    alivefav6 = factor(alivefav6, levels = c(1, 2), labels = c('Yes', 'No')),
    alivefav18 = factor(alivefav18, levels = c(1, 2), labels = c('Yes', 'No')),
    imputed6 = factor(imputed6, levels = c(1, 2), labels = c('Yes', 'No')),
    vis_infarct = factor(vis_infarct, levels = c(1, 2), labels = c('Yes', 'No')),
    
    
    # Variables with format YNM: (M is just a missing value, i.e. NA)
    deathdate_unknown = factor(deathdate_unknown, levels = c(1, 2), labels = c('Yes', 'No')),
    myocard_infarct = factor(myocard_infarct, levels = c(1, 2), labels = c('Yes', 'No')),
    extracranial_bleed = factor(extracranial_bleed, levels = c(1, 2), labels = c('Yes', 'No')),
    allergic_reaction = factor(allergic_reaction, levels = c(1, 2), labels = c('Yes', 'No')),
    other_effect = factor(other_effect, levels = c(1, 2), labels = c('Yes', 'No')),
    adverse_reaction = factor(adverse_reaction, levels = c(1, 2), labels = c('Yes', 'No')),
    R_atrophy = factor(R_atrophy, levels = c(1, 2), labels = c('Yes', 'No')),
    R_whitematter = factor(R_whitematter, levels = c(1, 2), labels = c('Yes', 'No')),
    R_oldlesion = factor(R_oldlesion, levels = c(1, 2), labels = c('Yes', 'No')),
    R_nonstroke_lesion = factor(R_nonstroke_lesion, levels = c(1, 2), labels = c('Yes', 'No')),
    
    
    # Variables with format YNDQ:
    livealone_rand = factor(livealone_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    indepinadl_rand = factor(indepinadl_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    nobleed_rand = factor(nobleed_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    antiplat_rand = factor(antiplat_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    atrialfib_rand = factor(atrialfib_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    liftarms_rand = factor(liftarms_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    ablewalk_rand = factor(ablewalk_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    weakface_rand = factor(weakface_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    weakarm_rand = factor(weakarm_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    weakleg_rand = factor(weakleg_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    dysphasia_rand = factor(dysphasia_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    hemianopia_rand = factor(hemianopia_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    visuospat_rand = factor(visuospat_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    brainstemsigns_rand = factor(brainstemsigns_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    otherdeficit_rand = factor(otherdeficit_rand, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    pred_nihss = factor(pred_nihss, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    other_antiplat_pre = factor(other_antiplat_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    aspirin_pre = factor(aspirin_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    dipyridamole_pre = factor(dipyridamole_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    clopidogrel_pre = factor(clopidogrel_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    lowdose_heparin_pre = factor(lowdose_heparin_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    fulldose_heparin_pre = factor(fulldose_heparin_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    warfarin_pre = factor(warfarin_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    antithromb_pre = factor(antithromb_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    hypertension_pre = factor(hypertension_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    diabetes_pre = factor(diabetes_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    stroke_pre = factor(stroke_pre, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    aspirin_day1 = factor(aspirin_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    antiplatelet_day1 = factor(antiplatelet_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    lowdose_heparin_day1 = factor(lowdose_heparin_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    full_anticoag_day1 = factor(full_anticoag_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    lowerBP_day1 = factor(lowerBP_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    nontrial_thromb_day1 = factor(nontrial_thromb_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    iv_fluids_day1 = factor(iv_fluids_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    insulin_day1 = factor(insulin_day1, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    aspirin_days2to7 = factor(aspirin_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    antiplatelet_days2to7 = factor(antiplatelet_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    lowdose_heparin_days2to7 = factor(lowdose_heparin_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    full_anticoag_days2to7 = factor(full_anticoag_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    lowerBP_days2to7 = factor(lowerBP_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    nontrial_thromb_days2to7 = factor(nontrial_thromb_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    nasogastric_days2to7 = factor(nasogastric_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    antibiotics_days2to7 = factor(antibiotics_days2to7, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    aspirin6 = factor(aspirin6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    aspirin18 = factor(aspirin18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    bloodthin6 = factor(bloodthin6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    bloodthin18 = factor(bloodthin18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    clotbust6 = factor(clotbust6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    clotbust18 = factor(clotbust18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    stocking6 = factor(stocking6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    stocking18 = factor(stocking18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    gotprobs6 = factor(gotprobs6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    gotprobs18 = factor(gotprobs18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    needhelp6 = factor(needhelp6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    needhelp18 = factor(needhelp18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    walkhelp6 = factor(walkhelp6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    walkhelp18 = factor(walkhelp18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    speakprob6 = factor(speakprob6, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    speakprob18 = factor(speakprob18, levels = c(1, 2, 20), labels = c('Yes', 'No', 'Question not answered')),
    
    
    # Variables with format "Y01N":
    recinfus = factor(recinfus, levels = c(0, 1), labels = c('No', 'Yes')),
    recR = factor(recR, levels = c(0, 1), labels = c('No', 'Yes')),
    recP = factor(recP, levels = c(0, 1), labels = c('No', 'Yes')),
    rec7 = factor(rec7, levels = c(0, 1), labels = c('No', 'Yes')),
    adjudicated = factor(adjudicated, levels = c(0, 1), labels = c('No', 'Yes')),
    sich7 = factor(sich7, levels = c(0, 1), labels = c('No', 'Yes')),
    dead7 = factor(dead7, levels = c(0, 1), labels = c('No', 'Yes')),
    recsix = factor(recsix, levels = c(0, 1), labels = c('No', 'Yes')),
    dead6mo = factor(dead6mo, levels = c(0, 1), labels = c('No', 'Yes')),
    
    
    # Rename categories
    R_scannorm = factor(R_scannorm, levels = c("N", "Y"), labels = c("No", "Yes")), 
    deathcode = factor(deathcode, levels = c("E1", "E2", "E3", "E4", "E7", "E8", "E9"), labels = c('Cerebrovascular', 'Cancer', 'Cardiovascular', 'Infection', 'Multiple Causes', 'Other', 'Unknown')),
    gender = factor(gender, levels = c(1, 2), labels = c('F', 'M')),
    infarct = factor(infarct, levels = c(0, 1, 2), labels = c('No', 'Possibly Yes', 'Definitely Yes')),
    itt_treat = factor(itt_treat, levels = c(0, 1), labels = c('rt-PA', 'Placebo')),
    stroketype = factor(stroketype, levels = c(1, 2, 3, 4, 5), labels = c('TACI', 'PACI', 'LACI', 'POCI', 'OTHER')),
    anticoag_pre = factor(anticoag_pre, levels = c(-1, 0, 1, 2, 3), labels = c('Unknown', 'None', 'Warfarin or other antthrombotic agent', 'Low dose heparin', 'Full dose heparin')),
    sevendaycase = factor(sevendaycase, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0), 
                          labels = c('Fatal massive swelling of original infarct', 'Fatal intracranial haemorrhage', 'Death from initial stroke - other',
                                     'Fatal recurrent ischaemic stroke', 'Fatal recurrent stroke of unknown type', 'Fatal non-cerebral event',
                                     'Nonfatal neurological deterioration, infarct swelling', 'Nonfatal symptomatic intracranial haemorrhage',
                                     'Nonfatal neurological deterioration not due to swelling or haemorrhage', 'Nonfatal recurrent ischaemic stroke',
                                     'Nonfatal recurrent stroke of unknown type', 'No event')),
    gcs_eye_rand = factor(gcs_eye_rand, levels = c(0, 1, 2, 3, 4, 10, 20, 30, 40), 
                          labels = c('Missing', 'Never', 'To Pain', 'To command', 'Spontaneously', 'Died so question not relevant', 'Question not answered', 
                                     'Form not returned', 'Question not asked')),
    gcs_eye_7 = factor(gcs_eye_7, levels = c(0, 1, 2, 3, 4, 10, 20, 30, 40), 
                       labels = c('Missing', 'Never', 'To Pain', 'To command', 'Spontaneously', 'Died so question not relevant', 'Question not answered', 
                                  'Form not returned', 'Question not asked')),
    gcs_motor_rand = factor(gcs_motor_rand, levels = c(0, 1, 2, 3, 4, 5, 6, 10, 20, 30, 40), 
                            labels = c('Missing', 'None', 'Extend to pain', 'Abnormal flex to pain', 'Normal flex to pain', 'Localises movements to pain', 
                                       'Normal', 'Died so question not relevant', 'Question not answered', 'Form not returned', 'Question not asked')),
    gcs_motor_7 = factor(gcs_motor_7, levels = c(0, 1, 2, 3, 4, 5, 6, 10, 20, 30, 40), 
                         labels = c('Missing', 'None', 'Extend to pain', 'Abnormal flex to pain', 'Normal flex to pain', 'Localises movements to pain', 
                                    'Normal', 'Died so question not relevant', 'Question not answered', 'Form not returned', 'Question not asked')),
    gcs_verbal_rand = factor(gcs_verbal_rand, levels = c(0, 1, 2, 3, 4, 5, 10, 20, 30, 40), 
                             labels = c('Missing', 'None', 'Noises only', 'Inappropriate words', 'Confused in time, place or person', 'Orientated in time, place and person', 
                                        'Died so question not relevant', 'Question not answered', 'Form not returned', 'Question not asked')),
    gcs_verbal_7 = factor(gcs_verbal_7, levels = c(0, 1, 2, 3, 4, 5, 10, 20, 30, 40), 
                          labels = c('Missing', 'None', 'Noises only', 'Inappropriate words', 'Confused in time, place or person', 'Orientated in time, place and person', 
                                     'Died so question not relevant', 'Question not answered', 'Form not returned', 'Question not asked')),
    
    liftarms_7 = factor(liftarms_7, levels = c(1, 2, 3, 4, 10, 20, 30, 40), 
                        labels = c('Yes', 'No', "Don't Know", 'Cannot assess', 'Died so question not relevant', 'Question not answered', 'Form not returned', 'Question not asked')),
    ablewalk_7 = factor(ablewalk_7, levels = c(1, 2, 3, 4, 10, 20, 30, 40), 
                        labels = c('Yes', 'No', "Don't Know", 'Cannot assess', 'Died so question not relevant', 'Question not answered', 'Form not returned', 'Question not asked')),
    indepinadl_7 = factor(indepinadl_7, levels = c(1, 2, 3, 4, 10, 20, 30, 40), 
                          labels = c('Yes', 'No', "Don't Know", 'Cannot assess', 'Died so question not relevant', 'Question not answered', 'Form not returned', 'Question not asked')),
    destination = factor(destination, levels = c(0, 1, 2, 3, 4, 5, 6, 7), 
                         labels = c('Still in hospital', 'Own home', 'Home of relative or friend', 'Nursing home', 
                                    'Residential home', 'Another hospital', 'Elsewhere', 'Died in hospital < 7 days')),
    findiag7 = factor(findiag7, levels = c(1, 2, 3), labels = c('Definite ischaemic stroke', 'Definite or probable haemorrhagic stroke', 'Non-stroke cause')),
    nonstroke_type7 = factor(nonstroke_type7, levels = c(1, 2, 3, 4), labels = c('Cerebral tumour', 'Migraine', 'Epilepsy', 'Other')),
    consent_type = factor(consent_type, levels = c(1, 2, 3, 4), labels = c('Patient signed consent', 'Patient verbal consent', 'Assent by relatives', 'Waiver of consent')), # there is also one "0" but in the documentation there is no 0 
    brainsite7 = factor(brainsite7, levels = c(0, 1, 2), labels = c('Unknown', 'Cerebral hemisphere', 'Posterior circulation')),
    haem_type7 = factor(haem_type7, levels = c(1, 2, 3), labels = c('Primary intracranial haemorrhage','Subdural haemorrhage','Subarachnoid haemorrhage')),
    sixmonthform = factor(sixmonthform, levels = c(1, 2), labels = c('Six month follow up form for patient at home', 'Six month follow up form for patient in hospital')),
    sixcompleted_by = factor(sixcompleted_by, levels = c(1, 2, 3), labels = c('Patient','Friend','Doctor')),
    mobility6 = factor(mobility6, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No problems walking', 'Some problems walking', 'Confined to bed', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    mobility18 = factor(mobility18, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No problems walking', 'Some problems walking', 'Confined to bed', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    selfcare6 = factor(selfcare6, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No problems with self care', 'Some problems washing or dressing', 'Unable to wash or dress self', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    selfcare18 = factor(selfcare18, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No problems with self care', 'Some problems washing or dressing', 'Unable to wash or dress self', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    activities6  = factor(activities6, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No problems with usual activities', 'Some problems with usual activities', 'Unable to perform usual activities', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    activities18  = factor(activities18, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No problems with usual activities', 'Some problems with usual activities', 'Unable to perform usual activities', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    pain6 = factor(pain6, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No pain or discomfort', 'Moderate pain or discomfort', 'Extreme pain or discomfort', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    pain18 = factor(pain18, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('No pain or discomfort', 'Moderate pain or discomfort', 'Extreme pain or discomfort', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    anxiety6 = factor(anxiety6, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('Not anxious or depressed', 'Moderately anxious or depressed', 'Extremely anxious or depressed', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    anxiety18 = factor(anxiety18, levels = c(1, 2, 3, 10, 20, 30, 40), labels = c('Not anxious or depressed', 'Moderately anxious or depressed', 'Extremely anxious or depressed', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked")),
    wherelive6 = factor(wherelive6, levels = c(1, 2, 3, 4, 10, 20, 30, 40, 50), labels = c('In my own home', 'In the home of a relative', 'In a residential home', 'In a nursing home', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked", 'Still in hospital so not relevant')),
    wherelive18 = factor(wherelive18, levels = c(1, 2, 3, 4, 10, 20, 30, 40, 50), labels = c('In my own home', 'In the home of a relative', 'In a residential home', 'In a nursing home', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked", 'Still in hospital so not relevant')),
    howlive6 = factor(howlive6, levels = c(1, 2, 10, 20, 30, 40, 50), labels = c('On my own', 'With my partner or relative', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked", 'Still in hospital so not relevant')),
    howlive18 = factor(howlive18, levels = c(1, 2, 10, 20, 30, 40, 50), labels = c('On my own', 'With my partner or relative', 'Died so question not relevant', 'Question not answered', "Form not returned", "Question not asked", 'Still in hospital so not relevant')),
    R_infarct_size = factor(R_infarct_size, levels = c(0, 1, 2, 3, 4), labels = c('None visible', 'Small', 'Medium', 'Large', 'Very large')), # missing is as well a category
    R_infarct_territory = factor(R_infarct_territory, levels = c(0, 1, 2, 3), labels = c('None', 'MCA or ACA or Borderzone', 'PCA or Cerebellar or Brainstem', 'Acute subcortical')), # missing is as well a category
    R_hypodensity = factor(R_hypodensity, levels = c(0, 1, 2), labels = c('None', 'Mild hypodensity', 'Severe hypodensity')),# missing is as well a category
    R_swelling = factor(R_swelling, levels = c(0, 1, 2, 3, 4), labels = c('None', 'Sulcal', 'Minor Ventricular', 'Moderate (C or D)', 'Severe (E or F)')),# missing is as well a category
    R_hyperdense_arteries = factor(R_hyperdense_arteries, levels = c(0, 1, 2), labels = c('None', 'Anterior', 'Posterior')),# missing is as well a category
    R_isch_change = factor(R_isch_change, levels = c(1, 2, 3), labels = c('Scan completely normal', 'Scan not normal but no sign of acute ischaemic change', 'Signs of acute ischaemic change')),
    yrfu_code = factor(yrfu_code, levels = c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9"), labels = c("Country not eligible", "Randomised after cut off date (30th June 2010)", "Patient or Relatives did not want to be contacted again/ withdrew consent", "Died before form could be obtained", "Emigrated", "GP asked us not to follow up patient due to poor health", "No trace/ No available contact details", "6 Month form obtained at close to 18 months, can be used instead", "Other")),
    waiver_code = factor(waiver_code, levels = c("A1", "A2", "A3", "A4", "A5", "A6"), labels = c("Aphasia", "Decreased level of consciousness", "Pre morbid cognitive impairment", "Neurological deterioration", "Executive consent", "Other")),
    extracranial_bleed_site = factor(extracranial_bleed_site, levels = c("C1", "C2", "C3", "C4", "C5", "C6"), labels = c("Gastrointestinal tract", "Intraabdominal", "Intramuscular",  "Epistaxis",  "Haematuria", "Other")),
    other_effect_code = factor(other_effect_code, levels = c("D1", "D2", "D21", "D22", "D23", "D24", "D5", "D6", "D7", "D8", "D9"), labels = c("Localised bruising e.g. around venepuncture sites/ sites where BP cuff applied", "Minor bleed", "epistaxis", "gastrointestinal", "gingival haemorrhage", "haematuria",  "Localised reaction to infusion", "Orolingual angioedma", "Anaphylaxis", "Seizures", "Other")),
    nostartcode = factor(nostartcode, levels = c("J1", "J11", "J12", "J19", "J2", "J3", "J4", "J5", "J6", "J9", "J91", "J92", "J99"), labels = c("Clinical decision not to give thrombolysis", "High BP", "Rapid improvement", "Other reason", "Patient or relative refused treatment", "Error in randomisation system", "Exclusion criteria identified post randomisation", "Delay between stroke onset and treatment was over 6 hours", "Clinical decision to give thrombolysis", "Additional treatment information", "Dose information", "Delay to treatment information", "Other information")),
    haltcode = factor(haltcode, levels = c( "I1", "I2", "I3", "I4", "I5", "I51", "I52", "I53", "I54", "I55", 
                                            "I56", "I57", "I59", "I6", "I61", "I62", "I63", "I69", "I7", 
                                            "I71", "I72", "I73", "I79", "I9"),
                      labels = c("Neurological Deterioration", "Bleeding", "Resite IV", "Allergic Reaction", 
                                 "Deterioration in Patients Condition", "Haemorrhage", "Low BP", "High BP", 
                                 "Vomiting", "Cardiac Arrest", "Loss of Consciousness", "Haematoma", 
                                 "Other Deterioration in Condition", "Technical IV Problem", "Change of Syringe", 
                                 "Blocked Syringe", "Machine Malfunction", "Other Technical Problem", 
                                 "Infusion Problem", "Delay during Infusion", "Incorrect Infusion Rate", 
                                 "Incorrect Infusion Dose", "Other Infusion Problem", "Other" )),
    asl = factor(asl, levels = c('B', 'L', 'M', 'R', 'U'), labels = c('Both sides', 'Left', 'Midline', 'Right', 'Unknown')),
    
    
    
    # Numeric -> Integer
    gcs_score_rand = as.integer(gcs_score_rand),
    med_adno = as.integer(med_adno),
    critcareno = as.integer(critcareno),
    strk_unitno = as.integer(strk_unitno),
    genwardno = as.integer(genwardno),
    ohs6 = as.integer(ohs6),
    ordinal6 = as.integer(ordinal6),
    event_days = as.integer(event_days),
    time_to_PH = as.integer(time_to_PH),
    R_mca_aspects = as.integer(R_mca_aspects),
    R_tot_aspects = as.integer(R_tot_aspects),
    
    # Character -> Factor
    randhosp_id = as.factor(randhosp_id),
    randpat_id = as.factor(randpat_id),
    nslesion2 = as.factor(nslesion2),
    nslesion3 = as.factor(nslesion3),
    nslesion4 = as.factor(nslesion4)
    
  )


# Some variables have missing values explicitly as a category
data_transformed_final <- data_transformed_3 %>%
  mutate(
    R_scannorm = factor(ifelse(is.na(R_scannorm), "Missing", as.character(R_scannorm))),
    R_hypodensity = factor(ifelse(is.na(R_hypodensity), "Missing", as.character(R_hypodensity))),
    destination = factor(ifelse(is.na(destination), "Missing", as.character(destination))),
    gcs_eye_rand = factor(ifelse(is.na(gcs_eye_rand), "Missing", as.character(gcs_eye_rand))),
    gcs_eye_7 = factor(ifelse(is.na(gcs_eye_7), "Missing", as.character(gcs_eye_7))),
    gcs_motor_rand = factor(ifelse(is.na(gcs_motor_rand), "Missing", as.character(gcs_motor_rand))),
    gcs_motor_7 = factor(ifelse(is.na(gcs_motor_7), "Missing", as.character(gcs_motor_7))),
    gcs_verbal_rand = factor(ifelse(is.na(gcs_verbal_rand), "Missing", as.character(gcs_verbal_rand))),
    gcs_verbal_7 = factor(ifelse(is.na(gcs_verbal_7), "Missing", as.character(gcs_verbal_7))),
    R_isch_change = factor(ifelse(is.na(R_isch_change), "Missing", as.character(R_isch_change))),
    R_infarct_size = factor(ifelse(is.na(R_infarct_size), "Missing", as.character(R_infarct_size))),
    R_swelling = factor(ifelse(is.na(R_swelling), "Missing", as.character(R_swelling))),
    R_infarct_territory = factor(ifelse(is.na(R_infarct_territory), "Missing", as.character(R_infarct_territory))),
    R_hyperdense_arteries = factor(ifelse(is.na(R_hyperdense_arteries), "Missing", as.character(R_hyperdense_arteries))),
    deathdate_unknown = factor(ifelse(is.na(deathdate_unknown), "Missing", as.character(deathdate_unknown))),
    myocard_infarct = factor(ifelse(is.na(myocard_infarct), "Missing", as.character(myocard_infarct))),
    extracranial_bleed = factor(ifelse(is.na(extracranial_bleed), "Missing", as.character(extracranial_bleed))),
    allergic_reaction = factor(ifelse(is.na(allergic_reaction), "Missing", as.character(allergic_reaction))),
    other_effect = factor(ifelse(is.na(other_effect), "Missing", as.character(other_effect))),
    adverse_reaction = factor(ifelse(is.na(adverse_reaction), "Missing", as.character(adverse_reaction))),
    R_atrophy = factor(ifelse(is.na(R_atrophy), "Missing", as.character(R_atrophy))),
    R_whitematter = factor(ifelse(is.na(R_whitematter), "Missing", as.character(R_whitematter))),
    R_oldlesion = factor(ifelse(is.na(R_oldlesion), "Missing", as.character(R_oldlesion))),
    R_nonstroke_lesion = factor(ifelse(is.na(R_nonstroke_lesion), "Missing", as.character(R_nonstroke_lesion))),
  )

check_final <- variable_summary(df = data, transformed_df = data_transformed_final)


# Save data ---------------------------------------------------------------
vars_medium <- c("outcome", "itt_treat", "nihss", "randdelay", "vis_infarct", "age", "gender","randhosp_id", "country", "trialphase", "dead7", "deathcode", "livealone_rand", "indepinadl_rand", "infarct", "antiplat_rand", "atrialfib_rand", "sbprand", "dbprand", "weight", "glucose", "ablewalk_rand", "gcs_eye_rand", "gcs_motor_rand", "gcs_verbal_rand", "gcs_score_rand", "stroketype", "randvioltype", "gotbolus", "infus_start", "infus_halt", "totdose", "aspirin_pre", "dipyridamole_pre", "lowdose_heparin_pre")
vars_small <- c("outcome", "itt_treat", "nihss", "randdelay", "vis_infarct", "age")

dat_big <- as.data.table(data_transformed_final)
dat_medium <- dat_big[, ..vars_medium]
dat_small <- dat_big[, ..vars_small]

saveRDS(dat_big, "Daten/data_big.Rds")
saveRDS(dat_medium, "Daten/data_medium.Rds")
saveRDS(dat_small, "Daten/data_small.Rds")