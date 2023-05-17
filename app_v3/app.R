# Загрузка необходимых библиотек
library(shiny)

# Старт Shiny приложения

ui = fluidPage(
    titlePanel("Предсказание обмана"),
    tabsetPanel(
      tabPanel("main",
               wellPanel(
                 textInput("income", "Доход"),
                 textInput("name_email_similarity", "Сходство имени и email"),
                 numericInput("prev_address_months_count", "Количество месяцев на предыдущем адресе", value = 123),
                 textInput("current_address_months_count", "Количество месяцев на текущем адресе"),
                 # Добавьте остальные поля здесь...
                 actionButton("go", "Готово")
               ),
               verbatimTextOutput("prediction")),
      tabPanel("info", "Здесь может быть информация о вашем приложении")
    )
  )
  
server = function(input, output, session) {
    
    library(dplyr)
  
    #xgb = load("xgb.RData")
    
    observe({
      if (input$income != "" & 
          input$name_email_similarity != "" & 
          input$prev_address_months_count != "" & 
          input$current_address_months_count != "" 
          # Добавьте остальные условия здесь...
      ) {
        output$prediction <- renderPrint({
          "Done"
          })
      } else{
        output$prediction <- renderPrint({
          #xgb %>% summary()
          input$prev_address_months_count
        })
      }
    })
    
    # prediction <- eventReactive(input$go, {
    #   newdata <- data.frame(
    #     income = as.numeric(input$income),
    #     name_email_similarity = as.numeric(input$name_email_similarity),
    #     prev_address_months_count = as.numeric(input$prev_address_months_count),
    #     current_address_months_count = as.numeric(input$current_address_months_count)
    #     # Добавьте остальные данные здесь...
    #   )
    #   predict(xgb, newdata, type = "response")
    # })
    # 
    # output$prediction <- renderPrint({
    #   prediction()
    # })
  }

shinyApp(ui = ui, server = server)
