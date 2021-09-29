#' Create clean, long dataset from data received from Roqua
#'
#' @param data A dataframe in pre-specified format (414 variables)
#' @return A clean dataframe that can be used for visualization
#' @import dplyr
#' @importFrom lubridate as_datetime
#' @export
prepare_data <- function(data) {

  should_include <- c("roqua_id", "patient_id", "gender", "birth_year", "respondent_id",
    "respondent_type", "respondent_label", "csp_dna_id", "csp_dna_protocol",
    "csp_dna_project", "csp_dna_measurement", "csp_dna_notes", "csp_dna_location",
    "csp_dna_invited_at", "csp_dna_emailed_at", "csp_dna_open_from",
    "csp_dna_non_response", "csp_dna_compl_by", "csp_dna_started_at",
    "csp_dna_completed_at", "csp_dna_date", "csp_dna_variant", "csp_dna_anonymous",
    "csp_dna_1", "csp_dna_2", "csp_dna_3", "csp_dna_4", "csp_dna_5",
    "csp_dna_6", "csp_dna_7", "csp_dna_8", "csp_dna_9", "csp_dna_10",
    "csp_dna_11", "csp_dna_12", "csp_dna_13", "csp_dna_14", "csp_dna_15",
    "csp_dna_16", "csp_dna_17", "csp_dna_18", "csp_dna_19", "csp_dna_20",
    "csp_dna_21", "csp_dna_22", "csp_dna_23", "csp_dna_24", "csp_dna_25",
    "csp_dna_26", "csp_dna_27", "csp_dna_28", "csp_dna_29", "csp_dna_30",
    "csp_dna_31", "csp_dna_32", "csp_dna_33", "csp_dna_34", "csp_dna_35",
    "csp_dna_36", "csp_dna_37", "csp_dna_38", "csp_dna_39", "csp_dna_40",
    "csp_dna_41", "csp_dna_42", "csp_dna_43", "csp_dna_44", "csp_dna_45",
    "csp_dna_46", "csp_dna_47", "csp_dna_48", "csp_dna_49", "csp_dna_50",
    "csp_dna_51", "csp_dna_52", "csp_dna_53", "csp_dna_54", "csp_dna_55_a0",
    "csp_dna_55_a1", "csp_dna_55_a2", "csp_dna_55_a3", "csp_dna_55_a4",
    "csp_dna_55_a5", "csp_dna_55_a6", "csp_dna_55_a7", "csp_dna_55_a8",
    "csp_dna_55_a9", "csp_dna_55a", "csp_dna_56", "csp_dna_56a",
    "csp_dna_57", "csp_dna_57a", "csp_dna_58", "csp_dna_58a_a1",
    "csp_dna_58a_a2", "csp_dna_58a_a3", "csp_dna_58a_a4", "csp_dna_58a_a5",
    "csp_dna_58a_a6", "csp_dna_58a_na", "csp_dna_59", "csp_dna_59a_a1",
    "csp_dna_59a_a2", "csp_dna_59a_a3", "csp_dna_59a_a4", "csp_dna_59a_a5",
    "csp_dna_59a_a6", "csp_dna_59a_na", "csp_dna_60", "csp_dna_60a_a1",
    "csp_dna_60a_a2", "csp_dna_60a_a3", "csp_dna_60a_a4", "csp_dna_60a_a5",
    "csp_dna_60a_a6", "csp_dna_60a_na", "csp_dna_61", "csp_dna_61a_a1",
    "csp_dna_61a_a2", "csp_dna_61a_a3", "csp_dna_61a_a4", "csp_dna_61a_a5",
    "csp_dna_61a_a6", "csp_dna_61a_na", "csp_dna_62", "csp_dna_62a_a1",
    "csp_dna_62a_a2", "csp_dna_62a_a3", "csp_dna_62a_a4", "csp_dna_62a_a5",
    "csp_dna_62a_a6", "csp_dna_62a_na", "csp_dna_63", "csp_dna_63a_a1",
    "csp_dna_63a_a2", "csp_dna_63a_a3", "csp_dna_63a_a4", "csp_dna_63a_a5",
    "csp_dna_63a_a6", "csp_dna_63a_a7", "csp_dna_63a_a8", "csp_dna_63a_a9",
    "csp_dna_63a_a10", "csp_dna_63a_a11", "csp_dna_63a_a12", "csp_dna_63a_a13",
    "csp_dna_63a_na", "csp_dna_64", "csp_dna_64a_a1", "csp_dna_64a_a2",
    "csp_dna_64a_a3", "csp_dna_64a_a4", "csp_dna_64a_a5", "csp_dna_64a_a6",
    "csp_dna_64a_a7", "csp_dna_64a_a8", "csp_dna_64a_a9", "csp_dna_64a_a10",
    "csp_dna_64a_a11", "csp_dna_64a_a12", "csp_dna_64a_a13", "csp_dna_64a_na",
    "csp_dna_65", "csp_dna_65a_a1", "csp_dna_65a_a2", "csp_dna_65a_a3",
    "csp_dna_65a_a4", "csp_dna_65a_a5", "csp_dna_65a_a6", "csp_dna_65a_a7",
    "csp_dna_65a_a8", "csp_dna_65a_a9", "csp_dna_65a_a10", "csp_dna_65a_a11",
    "csp_dna_65a_a12", "csp_dna_65a_a13", "csp_dna_65a_na", "csp_dna_66",
    "csp_dna_66a_a1", "csp_dna_66a_a2", "csp_dna_66a_a3", "csp_dna_66a_a4",
    "csp_dna_66a_na", "csp_dna_67", "csp_dna_67a_a1", "csp_dna_67a_a2",
    "csp_dna_67a_a3", "csp_dna_67a_a4", "csp_dna_67a_a5", "csp_dna_67a_a6",
    "csp_dna_67a_a7", "csp_dna_67a_a8", "csp_dna_67a_a9", "csp_dna_67a_a10",
    "csp_dna_67a_a11", "csp_dna_67a_a12", "csp_dna_67a_a13", "csp_dna_67a_a14",
    "csp_dna_67_a14a", "csp_dna_67a_na", "csp_dna_68", "csp_dna_68a_a1",
    "csp_dna_68a_a2", "csp_dna_68a_a3", "csp_dna_68a_a4", "csp_dna_68a_a5",
    "csp_dna_68a_a6", "csp_dna_68a_a7", "csp_dna_68a_a8", "csp_dna_68a_a9",
    "csp_dna_68a_a10", "csp_dna_68a_a11", "csp_dna_68a_a12", "csp_dna_68a_a13",
    "csp_dna_68a_a14", "csp_dna_68_a14a", "csp_dna_68a_na", "csp_dna_69",
    "csp_dna_69a_a1", "csp_dna_69a_a2", "csp_dna_69a_a3", "csp_dna_69a_a4",
    "csp_dna_69a_a5", "csp_dna_69a_a6", "csp_dna_69a_a7", "csp_dna_69a_a8",
    "csp_dna_69a_a9", "csp_dna_69a_a10", "csp_dna_69a_a11", "csp_dna_69a_a12",
    "csp_dna_69a_a13", "csp_dna_69a_a14", "csp_dna_69_a14a", "csp_dna_69a_na",
    "csp_dna_70", "csp_dna_70a_a1", "csp_dna_70a_a2", "csp_dna_70a_a3",
    "csp_dna_70a_a4", "csp_dna_70a_a5", "csp_dna_70a_a6", "csp_dna_70a_a7",
    "csp_dna_70a_a8", "csp_dna_70a_a9", "csp_dna_70a_a10", "csp_dna_70a_a11",
    "csp_dna_70a_a12", "csp_dna_70a_na", "csp_dna_71", "csp_dna_71a_a1",
    "csp_dna_71a_a2", "csp_dna_71a_a3", "csp_dna_71a_a4", "csp_dna_71a_a5",
    "csp_dna_71a_a6", "csp_dna_71a_na", "csp_dna_72", "csp_dna_72a_a1",
    "csp_dna_72a_a2", "csp_dna_72a_a3", "csp_dna_72a_a4", "csp_dna_72a_a5",
    "csp_dna_72a_a6", "csp_dna_72a_a7", "csp_dna_72a_a8", "csp_dna_72a_na",
    "csp_dna_73", "csp_dna_73a_a1", "csp_dna_73a_a2", "csp_dna_73a_a3",
    "csp_dna_73a_a4", "csp_dna_73a_a5", "csp_dna_73a_a6", "csp_dna_73a_a7",
    "csp_dna_73a_a8", "csp_dna_73a_na", "csp_dna_74", "csp_dna_74a_a1",
    "csp_dna_74a_a2", "csp_dna_74a_a3", "csp_dna_74a_a4", "csp_dna_74a_na",
    "csp_dna_75", "csp_dna_75a_a1", "csp_dna_75a_a2", "csp_dna_75a_a3",
    "csp_dna_75a_a4", "csp_dna_75a_na", "csp_dna_76", "csp_dna_76a_a1",
    "csp_dna_76a_a2", "csp_dna_76a_a3", "csp_dna_76a_a4", "csp_dna_76a_a5",
    "csp_dna_76a_a6", "csp_dna_76a_a7", "csp_dna_76a_a8", "csp_dna_76a_a9",
    "csp_dna_76a_a10", "csp_dna_76a_a11", "csp_dna_76a_a12", "csp_dna_76a_a13",
    "csp_dna_76a_a14", "csp_dna_76a_a15", "csp_dna_76a_a16", "csp_dna_76a_na",
    "csp_dna_77", "csp_dna_77a", "csp_dna_78", "csp_dna_78a", "csp_dna_fase",
    "csp_dna_droef", "csp_dna_droef_1", "csp_dna_droef_2", "csp_dna_droef_3",
    "csp_dna_droef_4", "csp_dna_droef_5", "csp_dna_droef_6", "csp_dna_droef_7",
    "csp_dna_droef_8", "csp_dna_blij", "csp_dna_blij_1", "csp_dna_blij_2",
    "csp_dna_blij_3", "csp_dna_blij_4", "csp_dna_blij_5", "csp_dna_boos",
    "csp_dna_boos_1", "csp_dna_boos_2", "csp_dna_boos_3", "csp_dna_boos_4",
    "csp_dna_bang", "csp_dna_bang_1", "csp_dna_bang_2", "csp_dna_bang_3",
    "csp_dna_bang_4", "csp_dna_bang_5", "csp_dna_bang_6", "csp_dna_bang_7",
    "csp_dna_energ", "csp_dna_energ_1", "csp_dna_energ_2", "csp_dna_energ_3",
    "csp_dna_span", "csp_dna_span_1", "csp_dna_span_2", "csp_dna_span_3",
    "csp_dna_span_4", "csp_dna_span_5", "csp_dna_span_6", "csp_dna_span_7",
    "csp_dna_span_8", "csp_dna_ondern", "csp_dna_ondern_1", "csp_dna_ondern_2",
    "csp_dna_contact", "csp_dna_contact_1", "csp_dna_contact_2",
    "csp_dna_contact_3", "csp_dna_zelfb", "csp_dna_zelfb_1", "csp_dna_zelfb_2",
    "csp_dna_zelfb_3", "csp_dna_zelfb_4", "csp_dna_zelfb_5", "csp_dna_zelfb_6",
    "csp_dna_zelfb_7", "csp_dna_zelfb_8", "csp_dna_contr", "csp_dna_contr_1",
    "csp_dna_contr_2", "csp_dna_contr_3", "csp_dna_contr_4", "csp_dna_contr_5",
    "csp_dna_contr_6", "csp_dna_gezel", "csp_dna_gezel_1", "csp_dna_locat",
    "csp_dna_locat_1", "csp_dna_doen", "csp_dna_doen_1", "csp_dna_terug",
    "csp_dna_terug_1", "csp_dna_terug_2", "csp_dna_terug_3", "csp_dna_zorg",
    "csp_dna_zorg_1", "csp_dna_zorg_2", "csp_dna_destr", "csp_dna_destr_1",
    "csp_dna_destr_2", "csp_dna_destr_3", "csp_dna_suic", "csp_dna_suic_1",
    "csp_dna_activ", "csp_dna_activ_1", "csp_dna_activ_2", "csp_dna_activ_3",
    "csp_dna_neur", "csp_dna_neur_1", "csp_dna_psych", "csp_dna_psych_1",
    "csp_dna_plicht", "csp_dna_plicht_1", "csp_dna_plicht_2", "csp_dna_onaard",
    "csp_dna_onaard_1", "csp_dna_onaard_2", "csp_dna_somat", "csp_dna_somat_1",
    "csp_dna_plezier", "csp_dna_plezier_1", "csp_dna_onplzr", "csp_dna_onplzr_1"
  )

  # If dataset does not have all pre-specified variables then stop
  if(!all(names(data) %in% should_include)) {
    stop("Dataset does not have prespecified variable list")
  }
  # If dataset has many completely missing questionnaires (>90%), then stop
  if( sum(is.na(data$csp_dna_non_response)) / nrow(data) < 0.10 ) {
    stop("Too many non-responses")
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
                    lichamelijke_kla, plezierig, onplezierig)), as.numeric)
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
      Onplezierig = rowMeans(select(., all_of(onplezierig)), na.rm = TRUE)
    )

  # If dataset has many missings on particular items (>50%), then stop
  if( (sum(is.nan(data$Bedroefd)) / nrow(data)) > 0.50  ) {
    stop("Too many non-responses on questions on bedroefd")
  }


  # New names of all variables in circle
  circle_vars_nms <- c("Bedroefd", "Blij", "Boos" , "Bang", "Energie",
                       "Spanning", "Ondernemen", "Contact_behoefte",
                       "Zelfbeeld", "Ervaren_controle", "Terugtrekken",
                       "Zorg_zelf", "Destructief", "Suicidaliteit",
                       "Activiteiten", "Onrustig", "Bijzondere_ervaringen",
                       "Verplichtingen", "Negatief_contact",
                       "Lichamelijke_klachten", "Plezierig", "Onplezierig")

  # Names of all variables in grid
  grid_nms <- c("csp_dna_58a_a1", "csp_dna_58a_a2", "csp_dna_58a_a3", "csp_dna_58a_a4",
                "csp_dna_58a_a5", "csp_dna_58a_a6", "csp_dna_61a_a1", "csp_dna_61a_a2",
                "csp_dna_61a_a3", "csp_dna_61a_a4", "csp_dna_61a_a5", "csp_dna_61a_a6",
                "csp_dna_65a_a1", "csp_dna_65a_a2", "csp_dna_65a_a3", "csp_dna_65a_a4",
                "csp_dna_65a_a5", "csp_dna_65a_a6", "csp_dna_65a_a7", "csp_dna_65a_a8",
                "csp_dna_65a_a9", "csp_dna_65a_a10", "csp_dna_65a_a11", "csp_dna_65a_a12",
                "csp_dna_65a_a13", "csp_dna_66a_a1", "csp_dna_66a_a2", "csp_dna_66a_a3",
                "csp_dna_66a_a4", "csp_dna_69a_a1", "csp_dna_69a_a2", "csp_dna_69a_a3",
                "csp_dna_69a_a4", "csp_dna_69a_a5", "csp_dna_69a_a6", "csp_dna_69a_a7",
                "csp_dna_69a_a8", "csp_dna_69a_a9", "csp_dna_69a_a10", "csp_dna_69a_a11",
                "csp_dna_69a_a12", "csp_dna_69a_a13", "csp_dna_69a_a14", "csp_dna_70a_a1",
                "csp_dna_70a_a2", "csp_dna_70a_a3", "csp_dna_70a_a4", "csp_dna_70a_a5",
                "csp_dna_70a_a6", "csp_dna_70a_a7", "csp_dna_70a_a8", "csp_dna_70a_a9",
                "csp_dna_70a_a10", "csp_dna_70a_a11", "csp_dna_70a_a12", "csp_dna_71a_a1",
                "csp_dna_71a_a2", "csp_dna_71a_a3", "csp_dna_71a_a4", "csp_dna_71a_a5",
                "csp_dna_71a_a6", "csp_dna_73a_a1", "csp_dna_73a_a2", "csp_dna_73a_a3",
                "csp_dna_73a_a4", "csp_dna_73a_a5", "csp_dna_73a_a6", "csp_dna_73a_a7",
                "csp_dna_73a_a8", "csp_dna_74a_a1", "csp_dna_74a_a2", "csp_dna_74a_a3",
                "csp_dna_74a_a4", "csp_dna_76a_a1", "csp_dna_76a_a2", "csp_dna_76a_a3",
                "csp_dna_76a_a4", "csp_dna_76a_a5", "csp_dna_76a_a6", "csp_dna_76a_a7",
                "csp_dna_76a_a8", "csp_dna_76a_a9", "csp_dna_76a_a10", "csp_dna_76a_a11",
                "csp_dna_76a_a12", "csp_dna_76a_a13", "csp_dna_76a_a14", "csp_dna_76a_a15",
                "csp_dna_76a_a16")

  # New names of all variables in grid
  grid_nms_mw <- c("Whatsapp", "Bellen", "Deurbel", "Smsen", "Afgezegd", "Werk_school_sport",
                   "Douchen", "Dag_nacht_ritme", "Eten", "Bewogen", "Recept_Medicatie",
                   "Medicatie_ingenomen", "Snijden", "Bonken_hoofd", "Krabben",
                   "Krassen", "Slaan_vuist", "Anderen_schade", "Spullen_kapot",
                   "Uitrekken_haren", "Alcohol_drugs", "Uitgeven_geld", "Gokken",
                   "Seksueel_risicovol", "Eetbui", "Passieve_gedachtes", "Actieve_gedachtes",
                   "Afscheidsbrief", "Concreet_plan", "TV", "Muziek", "Yoga", "Wandelen",
                   "Creatief", "Lezen", "Sporten", "Huishouden", "Spelletje", "Slapen",
                   "Contact_zoeken", "Afgesproken", "Buiten", "Anders", "Nagelbijten",
                   "Ijsberen", "Mouw_trekken", "Schoonmaken", "Roken", "Praten_anders",
                   "Uitpraten_niet", "Piekeren", "Dwanghandelingen", "Contact_veel",
                   "Alleen_niet", "Beslissingen_zelf", "Stemmen", "Schimmen", "Dissociaties",
                   "Achterdochtig", "Opdracht_krijgen", "Herbeleving", "Werken",
                   "Opleiding", "Kind_zorg", "Wassen", "Koken", "Schoonmaken_verplichting",
                   "Boodschappen", "Afspraken_nakomen", "Telefoon_onaardig", "Face_to_face_onaardig",
                   "Whatsapp_onaardig", "Social_media_onaardig", "Ademhalingsproblemen",
                   "Pijn_borst", "Hartkloppingen", "Misselijkheid", "Ontlasting",
                   "Buikpijn", "Hoofdpijn", "Duizeligheid", "Vermoeidheid", "Rugpijn",
                   "Spierpijn", "Tintelingen", "Zweten", "Veel_slapen", "Weinig_slapen",
                   "Nachtmerries")

  # Rename network variables into more sensible names
  data <- data %>%
    # Create date variables
    mutate(Datum = as_datetime(csp_dna_open_from)) %>%
    # Rename grid variables and make them numeric
    rename_at(vars(all_of(grid_nms)), ~ grid_nms_mw) %>%
    mutate_at(vars(all_of(grid_nms_mw)), as.numeric) %>%
    # Select only relevant variables
    select(all_of(c("Datum", circle_vars_nms, grid_nms_mw,
                    "csp_dna_55a", "csp_dna_56a",
                    "csp_dna_57a", "csp_dna_77a", "csp_dna_78a",
                    "csp_dna_fase")))

  # Create interval variables
  data$dayno <- time_count(data[["Datum"]], unit = "day")
  data$pertwee <- (data[["dayno"]] )  %/% 2

  return(data)
}



