#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
app_server <- function(input, output, session) {
  # Your application server logic
  stages = reactive(if_unselected(input$stages, all_tumor_stages))
  genders = reactive(if_unselected(tolower(input$gender), c('female', 'male')))
  races = reactive(if_unselected(input$race, all_races))
  vital_statuses = reactive(if_unselected(input$vital_status, c('Alive', 'Dead')))
  ages = reactive(input$age * days_per_year)

  data = reactive(
    dataset %>%
      filter(
        .data$tumor_stage %in% stages(),
        .data$gender %in% genders(),
        .data$race %in% races(),
        .data$vital_status %in% vital_statuses(),
        between(.data$age_at_diagnosis, ages()[1L], ages()[2L])
      ))

output$variant_classification = renderPlot({
  data() %>%
    mutate(Variant_Classification = order_by_count(.data$Variant_Classification)) %>%
    ggplot() +
    aes(y = .data$Variant_Classification, fill = .data$Variant_Classification) +
    geom_bar() +
    labs(x = '', y = '')
})

output$variant_type = renderPlot({
  data() %>%
    mutate(Variant_Type = order_by_count(.data$Variant_Type)) %>%
    ggplot() +
    aes(y = .data$Variant_Type) +
    geom_bar() +
    labs(x = '', y = '')
})

output$snv_class = renderPlot({
  data() %>%
    filter(
      .data$Variant_Type == 'SNP',
      .data$Reference_Allele %in% c('C', 'T')
    ) %>%
    mutate(SNV_Class = paste0(.data$Reference_Allele, '>', .data$Allele)) %>%
    ggplot() +
    aes(y = .data$SNV_Class) +
    geom_bar() +
    labs(x = '', y = '')
})

output$variants_per_sample = renderPlot({
  data() %>%
    mutate(
      Tumor_Sample_Barcode = order_by_count(.data$Tumor_Sample_Barcode, descending = TRUE),
      Variant_Classification = order_by_count(.data$Variant_Classification)
    ) %>%
    ggplot() +
    aes(x = .data$Tumor_Sample_Barcode, fill = .data$Variant_Classification) +
    geom_bar() +
    labs(x = '', y = '')
})

output$variant_summary = renderPlot({
  sample_var_counts = data() %>%
    count(.data$Tumor_Sample_Barcode, .data$Variant_Classification)

  sample_var_stats = sample_var_counts %>%
    group_by(.data$Variant_Classification) %>%
    dplyr::reframe(
      Names = c('min', 'lower', 'middle', 'upper', 'max'),
      Stats = boxplot.stats(.data$n, do.conf = FALSE, do.out = FALSE)$stats,
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = .data$Names, values_from = .data$Stats) %>%
    mutate(Variant_Classification = forcats::fct_reorder(
      .data$Variant_Classification, .data$middle, .desc = TRUE))

  sample_var_stats %>%
    ggplot() +
    aes(x = .data$Variant_Classification, color = .data$Variant_Classification) +
    geom_boxplot(
      aes(ymin = .data$min, lower = .data$lower, middle = .data$middle, upper = .data$upper, ymax = .data$max),
      stat = 'identity'
    ) +
    labs(x = '', y = '')
})

output$top_mutated_genes = renderPlot({
  top_10_genes = data() %>%
    count(.data$Hugo_Symbol) %>%
    slice_max(.data$n, n = 10L, with_ties = FALSE) %>%
    pull(.data$Hugo_Symbol)

  data() %>%
    filter(.data$Hugo_Symbol %in% top_10_genes) %>%
    mutate(
      Hugo_Symbol = order_by_count(.data$Hugo_Symbol),
      Variant_Classification = order_by_count(.data$Variant_Classification)
    ) %>%
    ggplot() +
    aes(y = .data$Hugo_Symbol, fill = .data$Variant_Classification) +
    geom_bar() +
    labs(x = '', y = '')
})
}
