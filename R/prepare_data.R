#' Create clean, long dataset from data received from Roqua
#'
#' @param data A dataframe in pre-specified format (265 variables)
#' @return A clean dataframe that can be used for visualization
#' @import dplyr
#' @importFrom lubridate as_datetime
#' @export
prepare_data <- function(data) {

  # If dataset does not have right number of variables, then stop
  if( !is.data.frame(data) ) {
    return("Data is not a dataset")
  }
  
  # If dataset does not have right number of variables, then stop
  if(length(names(data)) != 266 ) {
    return("Dataset does not have right number of variables (266)")
  }
  
  should_include <- c(
    "id", "respondent_id", "csp_dna_non_response", "csp_dna_open_from", 
    "csp_dna_fase", "csp_dna_1", "csp_dna_2", "csp_dna_3", "csp_dna_4", 
    "csp_dna_5", "csp_dna_6", "csp_dna_7", "csp_dna_8", "csp_dna_9", 
    "csp_dna_10", "csp_dna_11", "csp_dna_12", "csp_dna_13", "csp_dna_14", 
    "csp_dna_15", "csp_dna_16", "csp_dna_17", "csp_dna_18", "csp_dna_19", 
    "csp_dna_20", "csp_dna_21", "csp_dna_22", "csp_dna_23", "csp_dna_24", 
    "csp_dna_25", "csp_dna_26", "csp_dna_27", "csp_dna_28", "csp_dna_29", 
    "csp_dna_30", "csp_dna_31", "csp_dna_32", "csp_dna_33", "csp_dna_34", 
    "csp_dna_35", "csp_dna_36", "csp_dna_37", "csp_dna_38", "csp_dna_39", 
    "csp_dna_40", "csp_dna_41", "csp_dna_42", "csp_dna_43", "csp_dna_44", 
    "csp_dna_45", "csp_dna_46", "csp_dna_47", "csp_dna_48", "csp_dna_49", 
    "csp_dna_50", "csp_dna_51", "csp_dna_52", "csp_dna_53", "csp_dna_54", 
    "csp_dna_58", "csp_dna_59", "csp_dna_60", "csp_dna_61", "csp_dna_62", 
    "csp_dna_63", "csp_dna_64", "csp_dna_65", "csp_dna_66", "csp_dna_67", 
    "csp_dna_68", "csp_dna_69", "csp_dna_70", "csp_dna_71", "csp_dna_72", 
    "csp_dna_73", "csp_dna_74", "csp_dna_75", "csp_dna_76", "csp_dna_77", 
    "csp_dna_78", "csp_dna_58a_a1", "csp_dna_58a_a2", "csp_dna_58a_a3", 
    "csp_dna_58a_a4", "csp_dna_58a_a5", "csp_dna_58a_a6", "csp_dna_59a_a1", 
    "csp_dna_59a_a2", "csp_dna_59a_a3", "csp_dna_59a_a4", "csp_dna_59a_a5", 
    "csp_dna_59a_a6", "csp_dna_60a_a1", "csp_dna_60a_a2", "csp_dna_60a_a3", 
    "csp_dna_60a_a4", "csp_dna_60a_a5", "csp_dna_60a_a6", "csp_dna_61a_a1", 
    "csp_dna_61a_a2", "csp_dna_61a_a3", "csp_dna_61a_a4", "csp_dna_61a_a5", 
    "csp_dna_61a_a6", "csp_dna_62a_a1", "csp_dna_62a_a2", "csp_dna_62a_a3", 
    "csp_dna_62a_a4", "csp_dna_62a_a5", "csp_dna_62a_a6", "csp_dna_63a_a1", 
    "csp_dna_63a_a2", "csp_dna_63a_a3", "csp_dna_63a_a4", "csp_dna_63a_a5", 
    "csp_dna_63a_a6", "csp_dna_63a_a7", "csp_dna_63a_a8", "csp_dna_63a_a9", 
    "csp_dna_63a_a10", "csp_dna_63a_a11", "csp_dna_63a_a12", "csp_dna_63a_a13", 
    "csp_dna_63a_a14", "csp_dna_64a_a1", "csp_dna_64a_a2", "csp_dna_64a_a3", 
    "csp_dna_64a_a4", "csp_dna_64a_a5", "csp_dna_64a_a6", "csp_dna_64a_a7", 
    "csp_dna_64a_a8", "csp_dna_64a_a9", "csp_dna_64a_a10", "csp_dna_64a_a11", 
    "csp_dna_64a_a12", "csp_dna_64a_a13", "csp_dna_64a_a14", "csp_dna_65a_a1", 
    "csp_dna_65a_a2", "csp_dna_65a_a3", "csp_dna_65a_a4", "csp_dna_65a_a5", 
    "csp_dna_65a_a6", "csp_dna_65a_a7", "csp_dna_65a_a8", "csp_dna_65a_a9", 
    "csp_dna_65a_a10", "csp_dna_65a_a11", "csp_dna_65a_a12", "csp_dna_65a_a13", 
    "csp_dna_65a_a14", "csp_dna_66a_a1", "csp_dna_66a_a2", "csp_dna_66a_a3", 
    "csp_dna_66a_a4", "csp_dna_67a_a1", "csp_dna_67a_a2", "csp_dna_67a_a3", 
    "csp_dna_67a_a4", "csp_dna_67a_a5", "csp_dna_67a_a6", "csp_dna_67a_a7", 
    "csp_dna_67a_a8", "csp_dna_67a_a9", "csp_dna_67a_a10", "csp_dna_67a_a11", 
    "csp_dna_67a_a12", "csp_dna_67a_a13", "csp_dna_67a_a14", "csp_dna_68a_a1", 
    "csp_dna_68a_a2", "csp_dna_68a_a3", "csp_dna_68a_a4", "csp_dna_68a_a5", 
    "csp_dna_68a_a6", "csp_dna_68a_a7", "csp_dna_68a_a8", "csp_dna_68a_a9", 
    "csp_dna_68a_a10", "csp_dna_68a_a11", "csp_dna_68a_a12", "csp_dna_68a_a13", 
    "csp_dna_68a_a14", "csp_dna_69a_a1", "csp_dna_69a_a2", "csp_dna_69a_a3", 
    "csp_dna_69a_a4", "csp_dna_69a_a5", "csp_dna_69a_a6", "csp_dna_69a_a7", 
    "csp_dna_69a_a8", "csp_dna_69a_a9", "csp_dna_69a_a10", "csp_dna_69a_a11", 
    "csp_dna_69a_a12", "csp_dna_69a_a13", "csp_dna_69a_a14", "csp_dna_70a_a1", 
    "csp_dna_70a_a2", "csp_dna_70a_a3", "csp_dna_70a_a4", "csp_dna_70a_a5", 
    "csp_dna_70a_a6", "csp_dna_70a_a7", "csp_dna_70a_a8", "csp_dna_70a_a9", 
    "csp_dna_70a_a10", "csp_dna_70a_a11", "csp_dna_70a_a12", 
    "csp_dna_70a_a13", 
    "csp_dna_71a_a1", 
    "csp_dna_71a_a2", "csp_dna_71a_a3", "csp_dna_71a_a4", "csp_dna_71a_a5", 
    "csp_dna_71a_a6", "csp_dna_72a_a1", "csp_dna_72a_a2", "csp_dna_72a_a3", 
    "csp_dna_72a_a4", "csp_dna_72a_a5", "csp_dna_72a_a6", "csp_dna_72a_a7", 
    "csp_dna_72a_a8", "csp_dna_73a_a1", "csp_dna_73a_a2", "csp_dna_73a_a3", 
    "csp_dna_73a_a4", "csp_dna_73a_a5", "csp_dna_73a_a6", "csp_dna_73a_a7", 
    "csp_dna_73a_a8", "csp_dna_74a_a1", "csp_dna_74a_a2", "csp_dna_74a_a3", 
    "csp_dna_74a_a4", "csp_dna_75a_a1", "csp_dna_75a_a2", "csp_dna_75a_a3", 
    "csp_dna_75a_a4", "csp_dna_76a_a1", "csp_dna_76a_a2", "csp_dna_76a_a3", 
    "csp_dna_76a_a4", "csp_dna_76a_a5", "csp_dna_76a_a6", "csp_dna_76a_a7", 
    "csp_dna_76a_a8", "csp_dna_76a_a9", "csp_dna_76a_a10", "csp_dna_76a_a11", 
    "csp_dna_76a_a12", "csp_dna_76a_a13", "csp_dna_76a_a14", "csp_dna_76a_a15", 
    "csp_dna_76a_a16", "csp_dna_55a", "csp_dna_55_a0", "csp_dna_56a", 
    "csp_dna_57a", "csp_dna_77a", "csp_dna_78a", "csp_dna_79", "csp_dna_79a", 
    "csp_dna_79b")

  # If dataset does not have all pre-specified variables then stop
  if(!all(names(data) %in% should_include)) {
    return("Dataset does not have prespecified variable list")
  }
  # If dataset has many completely missing questionnaires (>90%), then stop
  # non-missing value on csp_dna_non_response means entire survey is missing for that date
  if( sum( !is.na(data$csp_dna_non_response) ) / nrow(data) > 0.90 ) {
    return("Entire surveys were skipped over 90% of the times")
  }
  # If dataset has fewer than 20 rows, then stop
  if( nrow(data) < 20 ) {
    return("Dataset has fewer than 20 rows")
  }
  

  # Vector with variable names of variables in circle-figure
  bedroefd = c("csp_dna_1", "csp_dna_2", "csp_dna_3", "csp_dna_4", "csp_dna_5",
               "csp_dna_6", "csp_dna_7", "csp_dna_8")
  blij = c("csp_dna_9", "csp_dna_10", "csp_dna_11", "csp_dna_12", "csp_dna_13")
  boos = c("csp_dna_14", "csp_dna_15", "csp_dna_16", "csp_dna_17")
  bang = c("csp_dna_18", "csp_dna_19", "csp_dna_20", "csp_dna_21",
           "csp_dna_22", "csp_dna_23", "csp_dna_24")
  energie = c("csp_dna_25", "csp_dna_26", "csp_dna_27")
  spanning = c("csp_dna_28", "csp_dna_29", "csp_dna_30", "csp_dna_31",
               "csp_dna_32", "csp_dna_33", "csp_dna_34", "csp_dna_35")
  ondernemen = c("csp_dna_36", "csp_dna_37")
  contact_beh = c("csp_dna_38", "csp_dna_39", "csp_dna_40")
  zelfbeeld = c("csp_dna_41", "csp_dna_42", "csp_dna_43", "csp_dna_44",
                "csp_dna_45", "csp_dna_46", "csp_dna_47", "csp_dna_48")
  erv_cont = c("csp_dna_49", "csp_dna_50", "csp_dna_51", "csp_dna_52",
               "csp_dna_53", "csp_dna_54")
  terugtrekken = c("csp_dna_58", "csp_dna_59", "csp_dna_60")
  zorg_zelf = c("csp_dna_61", "csp_dna_62")
  destructief = c("csp_dna_63", "csp_dna_64", "csp_dna_65")
  suicidaliteit = c("csp_dna_66")
  activiteiten = c("csp_dna_67", "csp_dna_68", "csp_dna_69")
  onrustig = c("csp_dna_70")
  bijzondere_erv = c("csp_dna_71")
  verplichtingen = c("csp_dna_72", "csp_dna_73")
  negatief_contact = c("csp_dna_74", "csp_dna_75")
  lichamelijke_kla = c("csp_dna_76")
  plezierig = c("csp_dna_77")
  onplezierig = c("csp_dna_78")
  slapen = c("csp_dna_79a")

  # Create network variables with new names
  # Can have multiple mutally exclusive possibilities so collapse into one
  # e.g., "blij" has five ways of measuring, but only 1 is used per client
  data <- data %>%
    mutate(
      # combine all vectors of variable names that need to become numeric
      across(all_of(c(bedroefd, blij, boos, bang, energie, spanning, ondernemen,
                    contact_beh, zelfbeeld, erv_cont, terugtrekken,
                    zorg_zelf, destructief, suicidaliteit, activiteiten,
                    onrustig, bijzondere_erv, verplichtingen, negatief_contact,
                    lichamelijke_kla, plezierig, onplezierig, slapen)), as.numeric)
    ) %>%
    mutate(
      Bedroefd = rowMeans(select(., all_of(bedroefd)), na.rm = TRUE),
      Blij = rowMeans(select(., all_of(blij)), na.rm = TRUE),
      Boos = rowMeans(select(., all_of(boos)), na.rm = TRUE),
      Bang = rowMeans(select(., all_of(bang)), na.rm = TRUE),
      Energie = rowMeans(select(., all_of(energie)), na.rm = TRUE),
      Spanning = rowMeans(select(., all_of(spanning)), na.rm = TRUE),
      Ondernemen = rowMeans(select(., all_of(ondernemen)), na.rm = TRUE),
      Contact_behoefte = rowMeans(select(., all_of(contact_beh)), na.rm = TRUE),
      Zelfbeeld = rowMeans(select(., all_of(zelfbeeld)), na.rm = TRUE),
      Ervaren_controle = rowMeans(select(., all_of(erv_cont)), na.rm = TRUE),
      Terugtrekken = rowMeans(select(., all_of(terugtrekken)), na.rm = TRUE),
      Zorg_zelf = rowMeans(select(., all_of(zorg_zelf)), na.rm = TRUE),
      Destructief = rowMeans(select(., all_of(destructief)), na.rm = TRUE),
      Suicidaliteit = rowMeans(select(., all_of(suicidaliteit)), na.rm = TRUE),
      Activiteiten = rowMeans(select(., all_of(activiteiten)), na.rm = TRUE),
      Onrustig = rowMeans(select(., all_of(onrustig)), na.rm = TRUE),
      Bijzondere_ervaringen = rowMeans(select(., all_of(bijzondere_erv)), na.rm = TRUE),
      Verplichtingen = rowMeans(select(., all_of(verplichtingen)), na.rm = TRUE),
      Negatief_contact = rowMeans(select(., all_of(negatief_contact)), na.rm = TRUE),
      Lichamelijke_klachten = rowMeans(select(., all_of(lichamelijke_kla)), na.rm = TRUE),
      Plezierig = rowMeans(select(., all_of(plezierig)), na.rm = TRUE),
      Onplezierig = rowMeans(select(., all_of(onplezierig)), na.rm = TRUE),
      Slapen = rowMeans(select(., all_of(slapen)), na.rm = TRUE)
    )


  # New names of all variables in circle
  circle_vars_nms <- c("Bedroefd", "Blij", "Boos" , "Bang", "Energie",
                       "Spanning", "Ondernemen", "Contact_behoefte",
                       "Zelfbeeld", "Ervaren_controle", "Terugtrekken",
                       "Zorg_zelf", "Destructief", "Suicidaliteit",
                       "Activiteiten", "Onrustig", "Bijzondere_ervaringen",
                       "Verplichtingen", "Negatief_contact",
                       "Lichamelijke_klachten", "Plezierig", "Onplezierig")
  
  # If dataset has many missings on particular items (>50%), then stop
  for(var in circle_vars_nms) {
    
    if( sum( !is.nan( data[ , var] ) ) == 0  ) {
      return(
        paste0("All missing values on variable ", var)
      )
    } else if( ( sum(is.nan( data[ , var] ) ) / nrow(data) ) > 0.50  ) {
      return(
        paste0("Too many non-responses (", round(100 * ( sum(is.nan( data[ , var] ) ) / nrow(data) )) , 
               "%) on variable ", var, "; should be over 50% complete")
      )
    }
  }
  
  
  # Long code to determine which variables were used and should be included in graphics
  # Names of variables Terugtrekken, "...a_na" not included
  tt <- c("csp_dna_58","csp_dna_59","csp_dna_60")
  # Name of variable that is not empty
  tt_sel <- tt[colSums(!is.na(data[, tt])) > 0]
  # Names of variables for grid
  tt_vars <- paste0(tt_sel, paste0("a_a", 1:6))
  
  # Names of variables Zelfzorg, "...a_na" not included
  zz <- c("csp_dna_61","csp_dna_62")
  # Name of variable that is not empty
  zz_sel <- zz[colSums(!is.na(data[, zz])) > 0]
  # Names of variables for grid
  zz_vars <- paste0(zz_sel, paste0("a_a", 1:6))
  
  # Names of variables Destructief, "...a_a14a" & "...a_na" not included
  de <- c("csp_dna_63",
          #"csp_dna_63a_a14", # nieuwe var
          "csp_dna_64","csp_dna_65")
  # Name of variable that is not empty
  de_sel <- de[colSums(!is.na(data[, de])) > 0]
  # Names of variables for grid
  de_vars <- paste0(de_sel, paste0("a_a", 1:14)) # should this be 15

  # Names of variables Suicidaliteit, "...a_na" not included
  su_vars <- c("csp_dna_66a_a1", "csp_dna_66a_a2", "csp_dna_66a_a3", "csp_dna_66a_a4")
  
  # Names of variables Activiteiten, "...a_a14a" & "...a_na" not included
  ac <- c("csp_dna_67","csp_dna_68","csp_dna_69")
  # Name of variable that is not empty
  ac_sel <- ac[colSums(!is.na(data[, ac])) > 0]
  # Names of variables for grid
  ac_vars <- paste0(ac_sel, paste0("a_a", 1:14))  
  
  # Names of variables Onrustig, "...a_a13", "...a_a13a" & "...a_na" not included
  on_vars <- c("csp_dna_70a_a1", "csp_dna_70a_a2", "csp_dna_70a_a3", 
               "csp_dna_70a_a4", "csp_dna_70a_a5", "csp_dna_70a_a6", 
               "csp_dna_70a_a7", "csp_dna_70a_a8", "csp_dna_70a_a9",
               "csp_dna_70a_a10", "csp_dna_70a_a11", "csp_dna_70a_a12",
               "csp_dna_70a_a13") # new addition

  
  # Names of variables Bijzondere / Psychiatrische ervaringen, 
  # "...a_na" not included
  be_vars <- c("csp_dna_71a_a1", "csp_dna_71a_a2", "csp_dna_71a_a3", 
               "csp_dna_71a_a4", "csp_dna_71a_a5", "csp_dna_71a_a6")
  
  # Names of variables Verplichtingen, "...a_na" not included
  ve <- c("csp_dna_72","csp_dna_73")
  # Name of variable that is not empty
  ve_sel <- ve[colSums(!is.na(data[, ve])) > 0]
  # Names of variables for grid
  ve_vars <- paste0(ve_sel, paste0("a_a", 1:8))  
  
  # Names of variables Negatief contact, "...a_na" not included
  nc <- c("csp_dna_74","csp_dna_75")
  # Name of variable that is not empty
  nc_sel <- nc[colSums(!is.na(data[, nc])) > 0]
  # Names of variables for grid
  nc_vars <- paste0(nc_sel, paste0("a_a", 1:4))  
  
  # Names of variables Lichamelijke klachten, "...a_na" not included 
  lc_vars <- c("csp_dna_76a_a1", "csp_dna_76a_a2", "csp_dna_76a_a3",
               "csp_dna_76a_a4", "csp_dna_76a_a5", "csp_dna_76a_a6", 
               "csp_dna_76a_a7", "csp_dna_76a_a8", "csp_dna_76a_a9", 
               "csp_dna_76a_a10", "csp_dna_76a_a11", "csp_dna_76a_a12", 
               "csp_dna_76a_a13", "csp_dna_76a_a14", "csp_dna_76a_a15",
               "csp_dna_76a_a16")
  
  
  
  
  # lichamelijke_kla = c("csp_dna_76")
  # plezierig = c("csp_dna_77")
  # onplezierig = c("csp_dna_78")
  
  # Names of all variables in grid
  grid_nms <- c(tt_vars, # names of 6 variables on terugtrekken
                zz_vars, # names of 6 variables on zelfzorg
                de_vars, # names of 14 variables on Destructief
                su_vars, # names of 4 variables on Suicidaliteit
                ac_vars, # names of 14 variables on Activiteiten
                on_vars, # names of 14 variables on Onrustig / Neurotisch
                be_vars, # names of 6 variables on Bijzondere ervaringen
                ve_vars, # names of 8 variables on Verplichtingen
                nc_vars, # names of 4 variables on Negatief contact
                lc_vars # names of 16 variables on lichamelijke klachten
                )

  # New names of all variables in grid
  grid_nms_mw <- c(
    # New names for terugtrekken
    "Whatsapp", "Bellen", "Deurbel", "Smsen", "Afgezegd", "Werk_school_sport",
    # New names for zelfzorg               
    "Douchen", "Dag_nacht_ritme", "Eten", "Bewogen", "Recept_Medicatie",
    "Medicatie_ingenomen", 
    # New names for destructief               
    "Snijden", "Bonken_hoofd", "Krabben",
    "Krassen", "Slaan_vuist", "Anderen_schade", "Spullen_kapot",
    "Uitrekken_haren", "Alcohol_drugs", "Uitgeven_geld", "Gokken",
    "Seksueel_risicovol", "Eetbui", 
    "Anders_destructief", # NIEUW
    # New names for suicidaliteit
    "Passieve_gedachtes", "Actieve_gedachtes",
    "Afscheidsbrief", "Concreet_plan", 
    # New names for activiteiten
    "TV", "Muziek", "Yoga", "Wandelen", "Creatief", "Lezen", "Sporten", 
    "Huishouden", "Spelletje", 
    "Slapen_act", 
    "Contact_zoeken", "Afgesproken", 
    "Buiten", "Anders", 
    # New names for onrustig
    "Nagelbijten", "Ijsberen", "Mouw_trekken", "Schoonmaken", "Roken", 
    "Praten_anders", "Uitpraten_niet", "Piekeren", "Dwanghandelingen", 
    "Contact_veel", "Alleen_niet", "Beslissingen_zelf", 
    "Anders_onrustig", # NIEUW
    # New names for Bijzondere ervaringen
    "Stemmen", "Schimmen", "Dissociaties", "Achterdochtig", 
    "Opdracht_krijgen", "Herbeleving", 
    # New names for Verplichtingen
    "Werken", "Opleiding", "Kind_zorg", "Wassen", "Koken", 
    "Schoonmaken_verplichting", "Boodschappen", "Afspraken_nakomen", 
    # New names for negatief ontact
    "Telefoon_onaardig", "Face_to_face_onaardig",
    "Whatsapp_onaardig", "Social_media_onaardig", 
    # New names for lichamelijke klachten
    "Ademhalingsproblemen", "Pijn_borst", "Hartkloppingen", "Misselijkheid", 
    "Ontlasting", "Buikpijn", "Hoofdpijn", "Duizeligheid", "Vermoeidheid", 
    "Rugpijn", "Spierpijn", "Tintelingen", "Zweten", "Veel_slapen", 
    "Weinig_slapen", "Nachtmerries")

  # Rename network variables into more sensible names
  data <- data %>%
    # Create date variables
    mutate(Datum = as_datetime(csp_dna_open_from, tz = "Europe/Amsterdam")) %>%
    # Rename grid variables and make them numeric
    rename_at(vars(all_of(grid_nms)), ~ grid_nms_mw) %>%
    mutate_at(vars(all_of(grid_nms_mw)), as.numeric) %>%
    # Select only relevant variables
    select(all_of(c("Datum", "csp_dna_non_response", 
                    circle_vars_nms, grid_nms_mw,
                    "csp_dna_55a", "csp_dna_55_a0", "csp_dna_56a",
                    "csp_dna_57a", "csp_dna_77a", "csp_dna_78a",
                    # Added sleep variables
                    "csp_dna_79",	"csp_dna_79a",	"csp_dna_79b",
                    "csp_dna_fase")))

  # Create interval variables
  data$dayno <- time_count(data[["Datum"]], unit = "day")
  data$pertwee <- (data[["dayno"]] ) %/% 2

  return(data)
}



