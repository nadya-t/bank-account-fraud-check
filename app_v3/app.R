library(shiny)

ui <- fluidPage(
  titlePanel("Предсказание обмана"),
  tabsetPanel(
    tabPanel("main",
             wellPanel(
               textInput("income", "Доход"),
               textInput("name_email_similarity", "Сходство имени и email"),
               textInput("prev_address_months_count", "Количество месяцев на предыдущем адресе"),
               textInput("current_address_months_count", "Количество месяцев на текущем адресе"),
               textInput("customer_age", "Возраст клиента"),
               selectInput("payment_type", "Тип платежа", choices = c("наличные", "карта", "электронные деньги")),
               textInput("bank_branch_count_8w", "Количество отделений банка за 8 недель"),
               textInput("date_of_birth_distinct_emails_4w", "Количество разных email адресов с датой рождения за 4 недели"),
               selectInput("employment_status", "Статус занятости", choices = c("работает", "не работает", "студент", "пенсионер")),
               textInput("credit_risk_score", "Кредитный рейтинг"),
               checkboxInput("email_is_free", "Это бесплатный email?"),
               selectInput("housing_status", "Статус жилья", choices = c("собственник", "арендатор", "социальное жилье")),
               checkboxInput("phone_home_valid", "Домашний телефон подтвержден?"),
               checkboxInput("phone_mobile_valid", "Мобильный телефон подтвержден?"),
               textInput("bank_months_count", "Количество месяцев в банке"),
               checkboxInput("has_other_cards", "Есть другие кредитные карты?"),
               textInput("proposed_credit_limit", "Предлагаемый кредитный лимит"),
               checkboxInput("foreign_request", "Запрос из-за границы?"),
               selectInput("device_os", "ОС устройства", choices = c("Windows", "macOS", "Linux", "Android", "iOS")),
               checkboxInput("keep_alive_session", "Сессия активна?"),
               textInput("device_distinct_emails_8w", "Количество разных email адресов с устройства за 8 недель"),
               textInput("device_fraud_count", "Количество обманных действий с устройства"),
               actionButton("go", "Готово")
             ),
             verbatimTextOutput("prediction")),
    tabPanel("info", "Здесь может быть информация о вашем приложении")
  )
)

server <- function(input, output) {
  prediction <- eventReactive(input$go, {
    if (input$go > 0) {
      newdata <- data.frame(income = as.numeric(input$income),
                            name_email_similarity = as.numeric(input$name_email_similarity),
                            prev_address_months_count = as.numeric(input$prev_address_months_count),
                            current_address_months_count = as.numeric(input$current_address_months_count),
                            customer_age = as.numeric(input$customer_age),
                            payment_type = as.character(input$payment_type),
                            bank_branch_count_8w = as.numeric(input$bank_branch_count_8w),
                            date_of_birth_distinct_emails_4w = as.numeric(input$date_of_birth_distinct_emails_4w),
                            employment_status = as.character(input$employment_status),
                            credit_risk_score = as.numeric(input$credit_risk_score),
                            email_is_free = as.logical(input$email_is_free),
                            housing_status = as.character(input$housing_status),
                            phone_home_valid = as.logical(input$phone_home_valid),
                            phone_mobile_valid = as.logical(input$phone_mobile_valid),
                            bank_months_count = as.numeric(input$bank_months_count),
                            has_other_cards = as.logical(input$has_other_cards),
                            proposed_credit_limit = as.numeric(input$proposed_credit_limit),
                            foreign_request = as.logical(input$foreign_request),
                            device_os = as.character(input$device_os),
                            keep_alive_session = as.logical(input$keep_alive_session),
                            device_distinct_emails_8w = as.numeric(input$device_distinct_emails_8w),
                            device_fraud_count = as.numeric(input$device_fraud_count))
      
      # Ваша модель для предсказания должна быть добавлена здесь
      # pred <- predict(your_model, newdata, type = "response")
      pred <- newdata 
      # "Ваше предсказание: " # Удалите эту строку и раскомментируйте строку выше, когда ваша модель будет готова
      return(pred)
    }
  })
  output$prediction <- renderPrint({ prediction() })
}

shinyApp(ui = ui, server = server)
