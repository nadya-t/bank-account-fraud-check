library(shiny)
library(shinyjs)
library(dplyr)

# Датасет и модель
id <- 1:100
income_y <- runif(100, 0, 1000)
months <- sample(1:60, 100, replace = TRUE)
age <- sample(seq(10, 100, by = 10), 100, replace = TRUE)
risk <- sample(-500:500, 100, replace = TRUE)
is_fraud <- sample(c(0, 1), 100, replace = TRUE) %>% as.factor()
data <- data.frame(id, income_y, months, age, risk, is_fraud)

model <- glm(is_fraud ~ . -id, data = data, family = binomial)

# Интерфейс
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
    textInput("income_y_input", "income_y", value = "", placeholder = "income_y..."),
    textInput("months_input", "months", value = "", placeholder = "months"),
    selectInput("age_input", "age", choices = seq(10, 100, by = 10), selected = NULL),
    textInput("risk_input", "risk", value = "", placeholder = "risk"),
    actionButton("info_submit", "Готово")
  ),
  
  textOutput("prediction")
)

server <- function(input, output, session) {
  
  observeEvent(c(input$id_submit, input$info_submit), {
    
    # Смотрим по ID
    if (input$buttons == 'id' && input$id_input != "") {
      id <- as.integer(input$id_input)
      validate(
        #Почему-то не видно вывода
        need(id %in% data$id, "Неверный id")
      )
      user_data <- data.frame(data[data$id == id,])
    } 
    
    # Смотрим по введенным параметрам
    else if (input$buttons == 'info' && input$income_y_input != "" &&
               input$months_input != "" && !is.null(input$age_input) &&
               input$risk_input != "") {
      
      income_y <- as.numeric(input$income_y_input)
      months <- as.integer(input$months_input)
      age <- as.integer(input$age_input)
      risk <- as.integer(input$risk_input)
      
      validate(
        # Не видно вывода тоже
        need(income_y >= 0 && income_y <= 1000, "Недопустимый income_y"),
        need(months >= 1 && months <= 60, "Недопустимый months"),
        need(age %in% seq(10, 100, by = 10), "Недопустимый age"),
        need(risk >= -500 && risk <= 500, "Недопустимый risk")
      )
      
      user_data <- data.frame(id = 1,
                              income_y = income_y,
                              months = months,
                              age = age,
                              risk = risk,
                              is_fraud = 1)
    }
    
    # Предсказание, вывод
    if (exists("user_data")) {
      prediction <- predict(model, newdata = user_data, type = "response")
      output$prediction <- renderText(ifelse(prediction > 0.5, "yes", "no"))
    }
  })
}

shinyApp(ui = ui, server = server)

