library(shiny)
library(tidymodels)
library(xgboost)
ui <- fluidPage(
  titlePanel("Определение мошеннического банковского аккаунта"),
  tabsetPanel(
    tabPanel("Ввод данных",
             wellPanel(
               numericInput("income", "Годовой доход клиента в количественных показателях, значения от 0 до 1", value = 1),
               numericInput("name_email_similarity", "Сходство имени и email, значения от 0 до 1, рассчитать по косинусному расстоянию", value = 1),
               numericInput("prev_address_months_count", "Количество месяцев регистрации на предыдущем месте жительства", value = 1),
               numericInput("current_address_months_count", "Количество месяцев регистрации на текущем месте жительства", value = 1),
               selectInput("customer_age", "Возраст клиента", choices = c("10", "20", "30", "40", "50", "60","70", "80", "90")),
               selectInput("payment_type", "Кредитный тип клиента", choices = c("AA", "AB", "AC", "AD", "AE")),
               numericInput("bank_branch_count_8w", "Число обращений клиента в выбранное отделение банка за последние 8 недель", value = 1),
               numericInput("date_of_birth_distinct_emails_4w", "Число электронных писем, полученных от заявителей с совпадающей с клиентом датой рождения за последние 4 недели", value = 0),
               selectInput("employment_status", "Тип занятости клиента", choices = c("CA", "CB", "CC", "CD", "CE", "CF", "CG")),
               numericInput("credit_risk_score", "Оценка риска заявления клиента, в промежутке от -500 до 500", value = 0),
               checkboxInput("email_is_free", "Домен электронной почты клиента бесплатный?"),
               selectInput("housing_status", "Текущий статус проживания клиента", choices = c("BA", "BB", "BC", "BD", "BE", "BF", "BG")),
               checkboxInput("phone_home_valid", "Указан и действителен ли домашний телефон клиента?"),
               checkboxInput("phone_mobile_valid", "Указан и действителен ли мобильный телефон клиента?"),
               numericInput("bank_months_count", "Сколько месяцев предыдущему аккаунту клиента, если он был; если данных нет или это его первый аккаунт, введите -1", value = -1),
               checkboxInput("has_other_cards", "Есть ли у клиента другие карты этой же банковской компании?"),
               numericInput("proposed_credit_limit", "Запрашиваемая клиентом сумму кредита в тысячах", value = 1),
               checkboxInput("foreign_request", "Клиент гражданин другой страны?"),
               selectInput("device_os", "Операционная система, с которой клиентом был сделан запрос", choices = c("windows", "linux", "macintosh", "x11", "other")),
               checkboxInput("keep_alive_session", "Остался ли клиент в аккаунте после подачи заявки?"),
               numericInput("device_distinct_emails_8w", "Количество электронных писем, присланных банку с устройства клиента за последние 8 недель", value = 0),
               numericInput("device_fraud_count", "Количество мошеннических обращений, присланных банку с устройства клиента", value = 0),
               actionButton("go", "Готово")
             ),
             verbatimTextOutput("prediction")),
    tabPanel("Результат", htmlOutput("info"))
  )
)

server <- function(input, output) {
  prediction <- eventReactive(input$go, {
    if (input$go > 0) {
      newdata <- data.frame(income = as.numeric(input$income),
                            name_email_similarity = as.numeric(input$name_email_similarity),
                            prev_address_months_count = as.integer(input$prev_address_months_count),
                            current_address_months_count = as.integer(input$current_address_months_count),
                            customer_age = as.integer(input$customer_age),
                            payment_type = as.character(input$payment_type),
                            bank_branch_count_8w = as.integer(input$bank_branch_count_8w),
                            date_of_birth_distinct_emails_4w = as.integer(input$date_of_birth_distinct_emails_4w),
                            employment_status = as.character(input$employment_status),
                            credit_risk_score = as.integer(input$credit_risk_score),
                            email_is_free = as.integer(input$email_is_free),
                            housing_status = as.character(input$housing_status),
                            phone_home_valid = as.integer(input$phone_home_valid),
                            phone_mobile_valid = as.integer(input$phone_mobile_valid),
                            bank_months_count = as.integer(input$bank_months_count),
                            has_other_cards = as.integer(input$has_other_cards),
                            proposed_credit_limit = as.integer(input$proposed_credit_limit),
                            foreign_request = as.integer(input$foreign_request),
                            device_os = as.character(input$device_os),
                            keep_alive_session = as.integer(input$keep_alive_session),
                            device_distinct_emails_8w = as.integer(input$device_distinct_emails_8w),
                            device_fraud_count = as.integer(input$device_fraud_count),
                            X = 1)
      
      load(url("https://docs.google.com/uc?id=1NF37j5QGD5Nj69MoY3EqyXeA5T2EqFn3&export=download&confirm=t"))
      # Ваша модель для предсказания должна быть добавлена здесь
      pred <- predict(xgb, newdata, type = "raw")
      
      # "Ваше предсказание: " # Удалите эту строку и раскомментируйте строку выше, когда ваша модель будет готова
      return(pred)
    }
  })
  #output$prediction <- renderPrint({print("Вероятность того, что аккаунт не мошеннический"); prediction()
  output$info <- renderUI({
    HTML(paste("Вероятность того, что аккаунт не мошеннический:", prediction()))
    
  })
  
}

shinyApp(ui = ui, server = server)
