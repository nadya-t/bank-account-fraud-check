library(shiny)
library(shinyjs)
library(dplyr)
library(DT)

id <- 1:100
income_y <- runif(100, 0, 1000)
months <- sample(1:60, 100, replace = TRUE)
age <- sample(seq(10, 100, by = 10), 100, replace = TRUE)
risk <- sample(-500:500, 100, replace = TRUE)
is_fraud <- sample(c(0, 1), 100, replace = TRUE) %>% as.factor()
data <- data.frame(id, income_y, months, age, risk, is_fraud)

model <- glm(is_fraud ~ . -id, data = data, family = binomial)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("id / info"),
  radioButtons("buttons", label = NULL, choices = c("id", "info"), selected = NULL, inline = TRUE),
  
  conditionalPanel(
    condition = "input.buttons == 'id'",
    textInput("id_input", "Введите id", placeholder = "id..."),
    actionButton("id_submit", "Готово")
  ),
  
  conditionalPanel(
    condition = "input.buttons == 'info'",
    DT::dataTableOutput('info_table'),
    actionButton("info_submit", "Готово")
  ),
  
  wellPanel(
    h3(textOutput("prediction"), align = "center")
  )
)

server <- function(input, output, session) {
  
  info_data <- reactiveVal(data.frame(
    income_y = NA,
    months = NA,
    age = NA,
    risk = NA,
    stringsAsFactors = FALSE
  ))
  
  output$info_table <- DT::renderDataTable(
    info_data(),
    editable = TRUE,
    rownames = FALSE,
    options = list(dom = 't')
  )
  
  observeEvent(c(input$id_submit, input$info_submit), {
    if (input$buttons == 'id' && input$id_input != "") {
      id <- as.integer(input$id_input)
      validate(
        need(id %in% data$id, "Неверный id")
      )
      user_data <- data.frame(data[data$id == id,])
    } else if (input$buttons == 'info') {
      validate(
        need(!is.na(info_data()$income_y), "Введите income_y"),
        need(!is.na(info_data()$months), "Введите months"),
        need(!is.na(info_data()$age), "Введите age"),
        need(!is.na(info_data()$risk), "Введите risk")
      )
      user_data <- data.frame(id = 1, info_data(), is_fraud = 1)
    }
    
    if (exists("user_data")) {
      prediction <- predict(model, newdata = user_data, type = "response")
      output$prediction <- renderText(ifelse(prediction > 0.5, "yes", "no"))
    }
  })
  
  proxy <- dataTableProxy('info_table')
  
  observeEvent(input$info_table_cell_edit, {
    info <- input$info_table_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value
    df <- info_data()
    df[i, j] <- ifelse(j == 1, as.numeric(v), as.integer(v))
    info_data(df)
    replaceData(proxy, info_data(), resetPaging = FALSE, rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
