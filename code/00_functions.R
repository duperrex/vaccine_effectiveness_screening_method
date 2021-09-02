## add_prop_test ----------------------------------------------------

#' wrapper around stats::prop.test to add proprortion with 95%ci in a data.table that allows to control with a multiplicator and appropriate rounding when facing very low proportions
#'
#'
#' @param x  Number with condition (numerator)
#' @param n  Total number (denominator)
#' @param multiplicator  Number to multiply the result so it is humanely readable. Default is 1. Suggested values : 10^3, 10^4, ...
#' @param rounding  Number of digits for rounding, to be adjusted according to multiplicator. Default is 4.
#' @param pct
#'
#'
#' @return new_cols_list List of columns with proportion and 95% confidence intervals that will be added in to the data.table
#'
#' @export
#'
#'
add_prop_test <-
  function (x,
            n,
            multiplicator = NULL,
            rounding = NULL,
            pct = TRUE) {
    if (is.null(multiplicator)) {
      multiplicator = 1
    }
    if (is.null(rounding)) {
      rounding = 4
    }
    if (is.null(pct) | multiplicator > 100) {
      pct = FALSE
    }

    t1 <- stats::prop.test(x, n)
    t2 <- setDT(broom::tidy(t1))
    t3 <- t2[, .(
      prop = estimate * multiplicator,
      conf_low = conf.low * multiplicator,
      conf_high = conf.high * multiplicator
    )]

    if (x == 0) {
      t3[, conf_low := NA]
      t3[, conf_high := NA]
    }

    if (pct == TRUE) {
      t4 <- t3[, lapply(.SD / multiplicator, formattable::percent, 1)]
    } else {
      t4 <- t3[, lapply(.SD, round, rounding)]
    }

    new_cols_list <- as.list(t4)
  }

# dt_1 <- fread("
#             x, n
#             89, 90000")
#
# dt_1[,c('prop', 'conf_low', 'conf_high') := add_prop_test(x, n)][]
#
# dt_1[,c('prop_per10K', 'conf_low_per10K', 'conf_high_per10K') := add_prop_test(x, n,
#                                                                                multiplicator = 10^4,
#                                                                                rounding = 2)][]
#
#
# dt_2 <- fread("
#             x, n
#             55, 1000")
#
#
# dt_2[, c('prop', 'conf_low', 'conf_high') := add_prop_test(x, n)][]
# dt_2[, c('prop_pct', 'conf_low_pct', 'conf_high_pct') := add_prop_test(x, n, 100, rounding = 1)][]



## add_RR_AR ----------------------------------------------

#' Add relative risk (RR) and absolute risk difference (AR) in a data.table
#'
#'
#'
#'
#' @param x1  Number of exposed with disease
#' @param n1  Total number of exposed
#' @param x2  Number of non-exposed with disease
#' @param n2  Total number of non-exposed
#'
#' @return new_cols_list List with RR, RR_low, RR_high , AR, AR_low, AR_high
#'
#' @export

add_RR_AR <- function (x1, n1, x2, n2, rounding = NULL, ...) {

  if (is.null(rounding)) {
    rounding = 2
  }

  aa <- x1
  bb <- n1 - x1
  cc <- x2
  dd <- n2 - x2

  dat <- as.table(matrix(c(aa, bb, cc, dd), nrow = 2, byrow = TRUE))
  colnames(dat) <- c("Yes", "No")
  rownames(dat) <- c("Yes", "No")
  cat("\n")
  print(dat)
  cat("\n")
  dat_epi <- epiR::epi.2by2(dat, ...)
  print(dat_epi)
  cat("\n")
  if (pubh::chisq.fisher(dat) == 1)
    print(stats::fisher.test(dat))
  else
    print(stats::chisq.test(dat))
  data_epi_tidy <- broom::tidy(dat_epi)
  # print(data_epi_tidy)
  # return(dat_epi)
  setDT(data_epi_tidy)
  rr_line <-
    data_epi_tidy[term %in% 'RR.strata.wald', .(RR = estimate,
                                                RR_low = conf.low,
                                                RR_high = conf.high)]
  rr_line <- round(rr_line, rounding)
  ar_line <-
    data_epi_tidy[term %in% 'ARisk.strata.wald', .(AR = estimate,
                                                   AR_low = conf.low,
                                                   AR_high = conf.high)]

  ar_line <- formattable::percent(ar_line/100, 4)

  new_cols <- setDT(c(rr_line, ar_line))
  new_cols_list <- as.list(new_cols)

}

# foo5b[, c('RR', 'RR_lower', 'RR_upper',
#           'AR', 'AR_lower', 'AR_upper') := add_RR_AR(x1 = Vac_1d_oui_Test_pos,
#                                                      n1 = Vac_1d_oui_Total,
#                                                      x2 = Vac_1d_non_Test_pos,
#                                                      n2 = Vac_1d_non_Total),
#       keyby = agegr_gen]
#
#
# ## add labels ----
# foo5b <- copy_labels(foo5b, foo5)
# sjlabelled::get_label(foo5b)
#
# # names(foo5b)
# foo5b <- foo5b %>%
#   sjlabelled::var_labels(
#     RR = 'Risque relatif',
#     RR_lower = 'RR IC95% inf',
#     RR_upper = 'RR IC95% sup',
#     AR = 'Différence de risque',
#     AR_lower = 'AR IC95% inf',
#     AR_upper = 'AR IC95% sup'
#   )
#
# sjlabelled::get_label(foo5b)

## vacovid_table_1 --------------------------------------------------
##
#' Prepare summary by variable v1 for data vacovid_ofsp. Uses valid_match_14d_post_2nd to count failures as numerator and total_persons_14d_post_second_vac as denominator. v1 needs to be quoted for now. Require last_import_sid to be created before.
##
##

vacovid_table_1 <- function(data, v1) {

   ## total_persons_14d_post_second_vac and vacc_failure by v1 on subset day_14_post_second_vac_date < last_import_sid
  (table_1 <- data[day_14_post_second_vac_date < last_import_sid, .(total_persons_14d_post_second_vac = .N,
                           vacc_failure = sum(valid_match_14d_post_2nd, na.rm = T)),
               keyby = v1])

  ## add prop and ci
  table_1[,
          c('vacc_failure_per10k',
            'conf_low_per10K',
            'conf_high_per10K') := add_prop_test(vacc_failure,
                                                 total_persons_14d_post_second_vac,
                                                 multiplicator = 10 ^ 4,
                                                 rounding = 1),
          keyby = v1][]


  ## convert integer cols to numeric so gt does alignement correctly
  integer_idx = which(sapply(table_1, is.integer))
  table_1[ , (integer_idx) := lapply(.SD, as.numeric), .SDcols = integer_idx]

}


## gt_vacovid_table_1 -----------------------------------------------
## title, subtitle and caption_table need to defined before
## Uses per 10'000 doses

gt_vacovid_table_1 <-
  function(data,
           persons_footnote = NULL,
           vacc_fail_footnote = NULL,
           per10k_footnote = NULL) {
    if (is.null(vacc_fail_footnote)) {
      vacc_fail_footnote <-
        "Cas malgré la vaccination complète =  nouveau cas avec un premier frottis positif 14 jours ou plus après la 2e dose"
    }
    if (is.null(per10k_footnote)) {
      per10k_footnote <-
        "Proportion des cas malgré la vaccination complète parmi les personnes avec 2 doses depuis 14j ou plus, et intervalles de confiance à 95%. exprimés en pour 10'000 personnes"
    }
    if (is.null(persons_footnote)) {
    persons_footnote <- "Personnes vaccinées avec 2 doses depuis 14j ou plus"
}
    data %>%
      gt() %>%
      tab_header(title = title,
                 subtitle = subtitle) %>%
      cols_label(
        total_persons_14d_post_second_vac = "Pers. avec 2 doses (N)",
        vacc_failure = "Cas vaccinés (N)",
        vacc_failure_per10k = "Cas vaccinés (pour 10 mille)",
        conf_low_per10K = "IC95% inf",
        conf_high_per10K = "IC95% sup"
      ) %>%
      fmt_number(columns = 2:3,
                 decimals = 0,
                 sep_mark = "'") %>%
      tab_source_note(source_note = caption_table) %>%
      tab_footnote(footnote = persons_footnote,
                   locations = cells_column_labels(columns = c('total_persons_14d_post_second_vac'))
      ) %>%
      tab_footnote(footnote = vacc_fail_footnote,
                   locations = cells_column_labels(columns = c('vacc_failure'))) %>%
      tab_footnote(footnote = per10k_footnote,
                   locations = cells_column_labels(
                     columns = c(
                       'vacc_failure_per10k',
                       'conf_low_per10K',
                       'conf_high_per10K'
                     )
                   )) %>%
      tab_options(data_row.padding = px(2))
  }




## gt_vacovid_table_2 -----------------------------------------------
## title, subtitle and caption_table need to defined before
## Uses per 10'000 doses

gt_vacovid_table_2 <-
  function(data,
           persons_footnote = NULL,
           vacc_fail_footnote = NULL,
           per10k_footnote = NULL) {
    if (is.null(vacc_fail_footnote)) {
      vacc_fail_footnote <-
        "Cas malgré la vaccination complète =  nouveau cas avec un premier frottis positif 14 jours ou plus après la 2e dose"
    }
    if (is.null(per10k_footnote)) {
      per10k_footnote <-
        "Proportion des cas malgré la vaccination complète parmi les personnes avec 2 doses depuis 14j ou plus, et intervalles de confiance à 95%. exprimés en pour 10'000 personnes"
    }
    if (is.null(persons_footnote)) {
      persons_footnote <- "Personnes vaccinées avec 2 doses depuis 14j ou plus"
    }
    data %>%

      gt() %>%
      tab_header(title = title,
                 subtitle = subtitle) %>%
      cols_label(
        total_persons_14d_post_second_vac = "Pers. avec 2 doses (N)",
        vacc_failure = "Cas vaccinés (N)",
        vacc_failure_per10k = "Cas vaccinés (pour 10 mille)",
        conf_low_per10K = "IC95% inf",
        conf_high_per10K = "IC95% sup"
      ) %>%
      fmt_number(columns = 3:4,
                 decimals = 0,
                 sep_mark = "'") %>%
      tab_source_note(source_note = caption_table) %>%
      tab_footnote(footnote = persons_footnote,
                   locations = cells_column_labels(columns = c('total_persons_14d_post_second_vac'))
      ) %>%
      tab_footnote(footnote = vacc_fail_footnote,
                   locations = cells_column_labels(columns = c('vacc_failure'))) %>%
      tab_footnote(footnote = per10k_footnote,
                   locations = cells_column_labels(
                     columns = c(
                       'vacc_failure_per10k',
                       'conf_low_per10K',
                       'conf_high_per10K'
                     )
                   )) %>%
      tab_options(data_row.padding = px(2))
  }


## vacovid_plot_1 ---------------------------------------------------
#' Generates the plot based on the table preparaed with vacovid_table_1()


#' @param data Summary table prepared with vacovid_table_1
#' @param v1 Variable
#' @param nudge_x Size of nudging by ggrepel

vacovid_plot_1 <- function(data, v1, nudge_x = NULL) {
  if (is.null(nudge_x)) {
    nudge_x = .3
  }

  ggplot(
    data,
    aes(
      x = get(v1),
      y = vacc_failure_per10k,
      ymin = conf_low_per10K,
      ymax = conf_high_per10K,
      label = vacc_failure_per10k
    )
  ) +
    # ylim(0, 100) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 20)) +
    geom_pointrange() +
    coord_flip() +
    labs(
      title = title,
      subtitle = subtitle,
      x = lab_x,
      y = lab_y,
      caption = caption_table
    ) +
    theme(plot.subtitle = ggtext::element_markdown()) +
    ggrepel::geom_text_repel(nudge_x = nudge_x, min.segment.length = Inf)
}


## vacovid_plot_2 ---------------------------------------------------
#' Generates the plot based on the table prepared with vacovid_table_1()


#' @param data Summary table prepared with vacovid_table_1
#' @param v1 Variable
#' @param nudge_x Size of nudging by ggrepel

vacovid_plot_2 <- function(data, v1, nudge_x = NULL) {
  if (is.null(nudge_x)) {
    nudge_x = .3
  }

  ggplot(
    data,
    aes(
      x = get(v1[2]),
      y = vacc_failure_per10k,
      ymin = conf_low_per10K,
      ymax = conf_high_per10K,
      label = vacc_failure_per10k
    )
  ) +
    # ylim(0, 100) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 20)) +
    geom_pointrange() +
    coord_flip() +
    labs(
      title = title,
      subtitle = subtitle,
      x = lab_x,
      y = lab_y,
      caption = caption_table
    ) +
    facet_wrap(. ~ get(v1[1])) +

    ggrepel::geom_text_repel(nudge_x = nudge_x, min.segment.length = Inf)
}


## od_gt_summary_layout -----
od_gt_summary_layout <- function(data, by =NULL, gtsave = NULL){

  gtsummary::tbl_summary(
    data = data,
    by = all_of(by), # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
    gtsummary::add_n() %>% # add column with total number of non-missing observations
    gtsummary::add_p() %>% # test for a difference between groups
    # gtsummary::modify_header(label = "**Variable**") %>% # update the column header
    gtsummary::bold_labels() %>%

    gtsummary::as_gt() %>%
    # replace NA by '-'
    gt::fmt_missing(columns = everything(),
                missing_text = "-") %>%
    gt::tab_header(title = title,
                   subtitle = subtitle_table) %>%
    gt::tab_source_note(source_note = caption_g) %>%
    gt::gtsave(table_name,
               # expand = 10,
               path = out_png4ppt)

}






## prepare_matching -------------------------------------------------
#' Prepare a string for matching by converting to snakecase (tolower), replacing punctuations as space, and replacing accents and others UTF-8 characters to ASCII

#' @param x  String or variable to prepare
#'
#' @return x_prep String or variable prepared
#'
#' @export



prepare_matching <- function(x) {
  x_prep <- tolower(x)
  x_prep <- stringr::str_replace_all(x_prep, "[[:punct:]]", " ")
  x_prep <- iconv(x_prep, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  return(x_prep)
}


aa <- 'TEST de NOM étendUS'
bb <- 'à accemt et nom-TIret'

prepare_matching(aa)
prepare_matching(bb)
#
# head(data_vacovid_subset$lastname)
#
# data_vacovid_subset[, lastname_prep := prepare_matching(lastname)]
# head(data_vacovid_subset$lastname_prep)



## prepare_matching_street ------------------------------------------
#' Prepare a string for matching by converting to snakecase (tolower), replacing punctuations as space, and replacing accents and others UTF-8 characters to ASCII

#' @param x  String or variable for street to prepare
#' @param txt  String of words qualifying the streets type to replace by a space (default keywords are in french)
#'
#' @return x_prep String or variable prepared
#'
#' @export
prepare_matching_street <- function(x, txt = NULL) {
  if (is.null(txt)){
    txt = 'allee|avenue|boulevard|chemin|route|place'
  }
  x_prep <- tolower(x)
  x_prep <- stringr::str_replace_all(x_prep, "[[:punct:]]", " ")
  x_prep <- stringr::str_replace_all(x_prep, "[[:digit:]]", " ")
  x_prep <- iconv(x_prep, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  x_prep <- stringr::str_replace_all(x_prep, txt, " ")
  x_prep <- stringr::str_squish(x_prep)
    x_prep <- ifelse(x_prep == "", NA, x_prep)
  return(x_prep)
}


stringr::str_squish('bonjour les     espaces     casses-pids')

cc <- 'Avenue de la Poste 25 et chemin part 45 avec boulevard du parc'
(cc1 <- prepare_matching_street(cc))
stringr::str_squish(cc1)

dd <- 'chemin de la poste'
prepare_matching_street(dd, txt = "route | chemin | boulevard | parc") # !! no spaces
prepare_matching_street(dd, txt = "route|chemin|boulevard|parc")

ee <- ""
prepare_matching_street(ee)

ff <- "    "
prepare_matching_street(ff)

gg <- "rte de la plage"
prepare_matching_street(gg)

hh <- "route de la porte et des Portes du Soleil"
prepare_matching_street(hh)


## longest_match ----
## https://stackoverflow.com/a/50454267/6176250
longest_match <- function(x, pattern) {
  matches <- stringr::str_match_all(x, pattern)
  purrr::map_chr(matches, ~ .[which.max(nchar(.))])
}

# # Then use it
#
# dt <- data.table(text = c('how is the biggest ??',
#                           'really amazing biggest stuff'))
#
#
# dt %>%
#   dplyr::mutate(mymatch = longest_match(text, "\\w+"))
#
#
# dt[, mymatch_2 := longest_match(text, "\\w+")][]


#' ## date_range -------------------------------------------------------
#' #' Return the range for a variable v1 of a data.table dt as start and end.
#'
#' #' @param dt A data.table
#' #' @param v1 A variable
#' #'
#' #'
#' #' @return start Start date
#' #' @return end End date
#' #'
#' #' @export
#' date_range <- function(dt, v1) {
#'
#' dt[!is.na(v1), .(
#'   start = min(v1),
#'   end = max(v1)
#' )]
#' }
#'
#' date_range(data_ofsp_detail_cases, `Date du prélèvement [Laboratoire]`)
#'
#' data_ofsp_detail_cases[!is.na(`Date du prélèvement [Laboratoire]`), range(`Date du prélèvement [Laboratoire]`)]




## every_second_item -----------------------------------------------------------------
## https://community.rstudio.com/t/how-to-automatically-skip-some-x-labels/65702
## xlabels <- sort(unique(df$x))
# xlabels[seq(2, length(xlabels), 2)] <- ""
#'
#' Assign a blank label for every other item in a list
#'
#' @param x A list
#'
#' @return x A list with a blank every second item
#'
#'@export

every_second_item <- function(x){
  x <- sort(unique(x))
  x[seq(2, length(x), 2)] <- ""
  x
}

