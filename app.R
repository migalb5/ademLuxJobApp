library(httr2)

library(shiny)
library(bslib)
library(purrr)
library(tibble)
library(DT)
library(shinycssloaders)


host = "http://localhost:8008"
# maybe read host and port from .Renviron file

bToken = "TokenExample1234567890"
# read Bearer Token from .Renviron file instead


filterByRowLimit <- textInput(
  inputId = "row_limit",
  label = "Row limit:",
  placeholder = "Type a number"
)

filterByCompanyId <- textInput(
  inputId = "company_id",
  label = "Company ID:",
  placeholder = "Number or blank"
)

filterByVacancyId <- textInput(
  inputId = "vacancy_id",
  label = "Vacancy ID:",
  placeholder = "Type ID string"
)

filterBySkillLabel <- textInput(
  inputId = "skill_label",
  label = "Skill label:",
  placeholder = "Type skill label name"
)

filterByCompanyName <- textInput(
  inputId = "company_name",
  label = "Company name:",
  placeholder = "Type company name"
)

filterByCanton <- textInput(
  inputId = "canton",
  label = "Canton:",
  placeholder = "Type a canton name"
)


cards <- list(
  card(
    full_screen = TRUE,
    #card_header("Companies"),
    layout_sidebar(
      sidebar = sidebar("Filter Companies", filterByRowLimit, filterByCompanyId),
      withSpinner(DTOutput("companies")),
      withSpinner(DTOutput("company_vacancies"))
    )
  ),
  card(
    full_screen = TRUE,
    #card_header("Vacancies"),
    layout_sidebar(
      sidebar = sidebar("Filter Vacancies", filterByVacancyId, filterBySkillLabel, filterByCompanyName, filterByCanton),
      withSpinner(DTOutput("vacancies"))
    )
  ),
  card(
    full_screen = TRUE,
    #card_header("Skills"),
    layout_sidebar(
      sidebar = sidebar("Filter Skills"),
      plotOutput("skills")
    )
  ),
  card(
    full_screen = TRUE,
    #card_header("Learning Tracks"),
    layout_sidebar(
      sidebar = sidebar("Filter Learning Tracks"),
      plotOutput("learning_tracks")
    )
  ),
  card(
    full_screen = TRUE,
    #card_header("Book Recommendations"),
    layout_sidebar(
      sidebar = sidebar("Filter Recommended Books"),
      plotOutput("books")
    )
  )
)

ui <- page_navbar(
  title = "ADEM luxJob Dashboard",
  nav_spacer(),
  nav_panel("Companies", cards[[1]]),
  nav_panel("Vacancies", cards[[2]]),
  nav_panel("Skills", cards[[3]]),
  nav_panel("Learning Tracks", cards[[4]]),
  nav_panel("Books", cards[[5]]),
)

# resp <- request("http://localhost:8008/skills") |>
#   req_auth_bearer_token("TokenExample1234567890") |>
#   req_url_query(limit = 5) |>    # <- add your query parameter here
#   req_perform()
#
# result <- resp |> resp_body_json()

call_api <- function(endpoint, token, ...) {
  request(endpoint) |>
    req_auth_bearer_token(token) |>
    req_url_query(...) |>
    req_perform() |>
    resp_body_json()
}

call_api_path <- function(base_endpoint,
                          token,
                          url_append,
                          url_encode = T) {
  req <- request(base_endpoint) |>
    req_auth_bearer_token(token)

  if (url_encode) {
    req <- req |>
      req_url_path_append(utils::URLencode(url_append, reserved = TRUE)) |>
      req_perform() |>
      resp_body_json()
  } else {
    req <- req |>
      req_url_path_append(url_append) |>
      req_perform() |>
      resp_body_json()
  }
  return(req)
}

call_api_post <- function(endpoint, token, ...) {
  request(endpoint) |>
    req_auth_bearer_token(token) |>
    req_method("POST") |>
    req_body_json(list(...)) |>
    req_perform() |>
    resp_body_json()
}

server <- function(input, output) {

# Companies ----------------------------------------------------------------------------------------------------

  df_companies <- reactive({
    if (nchar(input$company_id) > 0) {
      result <- call_api_path(paste(host, "/companies", sep = ""), bToken, input$company_id)
      call_api_post(paste(host, "/log_search", sep = ""), bToken, user_id = 10, query = paste("/companies/", input$company_id, sep = ""))
      df_comp <- result[[1]] %>%
        map(~ tibble(
          company_id = .x$company_id,
          name = .x$name,
          sector = .x$sector
        )) %>%
        list_rbind()
      df_comp_vac <- result[[2]] %>%
        map(~ tibble(
          vacancy_id = .x$vacancy_id,
          canton = .x$canton,
          occupation = .x$occupation,
          year = .x$year,
          month = .x$month
        )) %>%
        list_rbind()
      return(list(df_comp, df_comp_vac))
    } else {
      result <- call_api(paste(host, "/companies", sep = ""), bToken, limit = as.integer(input$row_limit))
      call_api_post(paste(host, "/log_search", sep = ""), bToken, user_id = 10, query = paste("/companies?limit=", input$row_limit, sep = ""))
      df_comp <- result %>%
        map(~ tibble(
          company_id = .x$company_id,
          name = .x$name,
          sector = .x$sector
        )) %>%
        list_rbind()
      return(list(df_comp, NA))
    }
  })

  output$companies <- renderDataTable({
    datatable(df_companies()[[1]], filter = "top", rownames = FALSE, caption = "Company details", options = list(pageLength = 4))
  })

  output$company_vacancies <- renderDataTable({
    if (is.data.frame(df_companies()[[2]])) {
      datatable(df_companies()[[2]], filter = "top", rownames = FALSE, caption = paste("Vacancies advertised by: ", df_companies()[[1]]["name"], sep = ""), options = list(pageLength = 10))
    }
  })

# Vacancies ----------------------------------------------------------------------------------------------------

  # output$vacancies <- renderDataTable({
  #   datatable(df_vacancies(), filter = "top", rownames = FALSE, options = list(pageLength = 4))
  # })

# Skills ----------------------------------------------------------------------------------------------------

  # output$skills <- renderDataTable({
  #   datatable(df_skills(), filter = "top", rownames = FALSE, options = list(pageLength = 4))
  # })

# Learning Tracks ----------------------------------------------------------------------------------------------------

  # output$learning_tracks <- renderDataTable({
  #   datatable(df_learning_tracks(), filter = "top", rownames = FALSE, options = list(pageLength = 4))
  # })

# Books ----------------------------------------------------------------------------------------------------

  # output$books <- renderDataTable({
  #   datatable(df_books(), filter = "top", rownames = FALSE, options = list(pageLength = 4))
  # })
}

shinyApp(ui, server)


