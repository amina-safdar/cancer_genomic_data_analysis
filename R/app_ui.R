#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel(h1("XATTACA TCGA-BRCA explorer")),

      wellPanel(

        fluidRow(
          column(
            width = 4,
            selectizeInput(
              "stages",
              "Tumor stage",
              setNames(all_tumor_stages, sub('^stage ', '', all_tumor_stages)),
              multiple = TRUE
            )
          ),
          column(
            width = 4,
            checkboxGroupInput(
              'gender', 'Gender',
              c('Female', 'Male')
            )
          ),
          column(
            width = 4,
            selectizeInput('race', 'Race', all_races, multiple = TRUE)
          )
        ),

        fluidRow(
          column(
            width = 4,
            checkboxGroupInput(
              'vital_status', 'Vital status',
              c('Alive', 'Dead')
            )
          ),
          column(
            width = 8,
            sliderInput(
              'age', 'Age',
              min(all_ages), max(all_ages), range(all_ages), step = 1L
            )
          )
        )
      ),

      fluidRow(
        small_plot('variant_classification'),
        small_plot('variant_type'),
        small_plot('snv_class', 'SNV class'),
        small_plot('variants_per_sample'),
        small_plot('variant_summary'),
        small_plot('top_mutated_genes')
      )
    )
  )
}
#' Create a small Shiny plot output
#'
#' @param id the plot ID as a character string
#' @param title (optional) the plot title; if missing, inferred from the `id`
small_plot = function (id, title = id_to_title(id), output = plotly::plotlyOutput) {
  column(4, h3(title), plotOutput(id, height = '200px'))
}

id_to_title = function (id) {
  stringr::str_to_title(gsub('_', ' ', id))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "brca.shiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
