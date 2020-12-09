function(input, output, session) {
  
  # Dados selecionados
  
  selectedData_uf <- reactive({
    dados_uf %>%
      filter(uf_name == input$uf)
  })
  
  capital <- reactive({
    case_when(
      input$uf == "Acre" ~ "Rio Branco",
      input$uf == "Amapá" ~ "Macapá",
      input$uf == "Amazonas" ~ "Manaus",
      input$uf == "Pará" ~ "Belém",
      input$uf == "Rondônia" ~ "Porto Velho",
      input$uf == "Roraima" ~ "Boa Vista",
      input$uf == "Tocantins" ~ "Palmas",
      input$uf == "Alagoas" ~ "Maceió",
      input$uf == "Bahia" ~ "Salvador",
      input$uf == "Ceará" ~ "Fortaleza",
      input$uf == "Maranhão" ~ "São Luís",
      input$uf == "Paraíba" ~ "João Pessoa",
      input$uf == "Pernambuco" ~ "Recife",
      input$uf == "Piauí" ~ "Teresina",
      input$uf == "Rio Grande do Norte" ~ "Natal",
      input$uf == "Sergipe" ~ "Aracaju",
      input$uf == "Distrito Federal" ~ "Brasília",
      input$uf == "Goiás" ~ "Goiânia",
      input$uf == "Mato Grosso" ~ "Cuiabá",
      input$uf == "Mato Grosso do Sul" ~ "Campo Grande",
      input$uf == "Espírito Santo" ~ "Vitória",
      input$uf == "Minas Gerais" ~ "Belo Horizonte",
      input$uf == "Rio de Janeiro" ~ "Rio de Janeiro",
      input$uf == "São Paulo" ~ "São Paulo",
      input$uf == "Paraná" ~ "Curitiba",
      input$uf == "Rio Grande do Sul" ~ "Porto Alegre",
      input$uf == "Santa Catarina" ~ "Florianópolis"
    )
  })
  
  selectedData_city <- reactive({
    dados_capitais %>%
      filter(city == capital())
  })
  
  ##### Gráficos #####
  output$plot1 <- renderPlot({
    if (input$metrics == "Novos Casos") {
      df <- filter(selectedData_uf(), confirmed >= mincases)
      dataset <- data.frame(n0 = as.numeric(df$new_confirmed),
                            n = as.numeric(df$new_confirmed),
                            days = 1:length(as.numeric(df$new_confirmed)),
                            date = df$date)
      t1 <- "Previsão de Novos Casos de Covid-19"
    } else {
      df <- filter(selectedData_uf(), deaths >= mindeaths)
      dataset <- data.frame(n0 = as.numeric(df$new_deaths),
                            n = as.numeric(df$new_deaths),
                            days = 1:length(as.numeric(df$new_deaths)),
                            date = df$date)
      t1 <- "Previsão de Novas Mortes por Covid-19"
    }
    
    for (i in 1:nrow(dataset)) {
      if (i < 2) {
        dataset$n[i] <- ifelse(dataset$n[i] <= 0, dataset$n[i+1], dataset$n[i])
      } else if (i < 8) {
        dataset$n[i] <- ifelse(dataset$n[i] <= 0, mean(dataset$n[c(i-1,i+1)]), dataset$n[i])
      } else {
        dataset$n[i] <- ifelse(dataset$n[i] <= 0, mean(dataset$n[(i-7):(i-1)]), dataset$n[i])
      }
    }
    
    dataset <- dataset %>%
      mutate(sun = 1*I(weekdays(date) == "domingo"),
             mon = 1*I(weekdays(date) == "segunda")) %>%
      select(-date)
    
    fit <- gamlss(log(n) ~ cs(days)+sun+mon, data = dataset, family = LO)
    nhat <- exp(fit$mu.fv)
    # coefs = fit$mu.coefficients
    aux = seq((length(dataset$days)+1),(length(dataset$days)+input$janela), 1)
    future = aux
    future.days = weekdays(seq(df$date[length(df$date)]+1,
                               df$date[length(df$date)]+input$janela, 1))
    sun = 1*I(future.days == "domingo")
    mon = 1*I(future.days == "segunda")
    
    dataset <<- dataset
    
    predn <- as.numeric(predict(
      fit,
      what = "mu",
      newdata = data.frame(days = future,
                           sun = sun,
                           mon = mon),
      type = "response"
    ))
    
    sqr <- sum(fit$residuals^2)
    nvar <- ncol(dataset)-2
    nobs <- nrow(dataset)
    quant <- qt(.975, nobs-nvar-1)
    mobs <- as.matrix(cbind(1, dataset[,-c(1,2)]))
    mfut <- as.matrix(cbind(1, future, sun, mon))
    prodx <- t(mobs)%*%mobs
    w <- ginv(prodx)
    v <- c()
    for (i in 1:nrow(mfut)) {
      v[i] <- t(mfut[i,])%*%w%*%mfut[i,]
    }
    merror <- quant*sqrt((1/nobs)*sqr*v)
    l <- exp(predn-merror)
    u <- exp(predn+merror)
    
    dataset <<- data.frame(dataset, nhat)
    new.dataset <<- data.frame(predn = exp(predn), days = future, l = l, u = u)
    last.day <- last(dataset$days)
    
    plt <<- ggplot() +
      geom_line(data = dataset, aes(x = days, y = nhat, color = "fitted", linetype = "fitted"),
                size = .75) +
      geom_point(data = dataset, aes(x = days, y = n0), color = "red4") +
      geom_line(data = dataset, aes(x = days, y = n0, color = "obs", linetype = "obs")) +
      geom_line(data = new.dataset, aes(x = days, y = predn, color = "predicted",
                                        linetype = "predicted"), size = .75) +
      geom_vline(xintercept = last.day, color = "darkgrey") +
      geom_ribbon(data = new.dataset, aes(x = days, ymin = l, ymax = u, fill = "cband"),
                  alpha = .3) +
      xlim(0, 500) +
      scale_color_manual(
        name = "",
        values = c("fitted" = "#D55E00",
                   "obs" = "red4",
                   "predicted" = "darkblue"),
        labels = c("Valores Ajustados",
                   "Valores Observados",
                   "Valores Preditos")
      ) +
      scale_fill_manual(
        name = "",
        values = c("cband" = "grey40"),
        labels = c("IC 95%")
      ) +
      scale_linetype_manual(
        name = "",
        values = c("fitted" = "solid",
                   "obs" = "solid",
                   "predicted" = "dashed"),
        labels = c("Valores Ajustados",
                   "Valores Observados",
                   "Valores Preditos")
      ) +
      labs(
        x = ifelse(input$metrics == "Novos Casos",
                   "Dias desde o 100º caso",
                   "Dias desde a 50ª morte"),
        y = paste("Número de", input$metrics),
        title = paste0(t1, ", ", input$uf),
        subtitle = paste0("Atualizado em ", format(hoje, "%d/%m/%Y")),
        caption = "Fonte: http://obsrpb.com.br/ufpb/"
      ) +
      theme_bw() +
      theme(legend.position = c(.9, .8),
            legend.title = element_blank(),
            legend.background = element_blank(),
            legend.spacing = unit(-3.15, "cm"))
    
    plt
  })
  
  output$plot2 <- renderPlot({
    if (input$metrics == "Novos Casos") {
      df2 <- filter(selectedData_city(), confirmed >= mincases)
      dataset2 <- data.frame(n0 = as.numeric(df2$new_confirmed),
                             n = as.numeric(df2$new_confirmed),
                             days = 1:length(as.numeric(df2$new_confirmed)),
                             date = df2$date)
      t1 <- "Previsão de Novos Casos de Covid-19"
    } else {
      df2 <- filter(selectedData_city(), deaths >= mindeaths)
      dataset2 <- data.frame(n0 = as.numeric(df2$new_deaths),
                             n = as.numeric(df2$new_deaths),
                             days = 1:length(as.numeric(df2$new_deaths)),
                             date = df2$date)
      t1 <- "Previsão de Novas Mortes por Covid-19"
    }
    
    for (i in 1:nrow(dataset2)) {
      if (i < 2) {
        dataset2$n[i] <- ifelse(dataset2$n[i] <= 0, dataset2$n[i+1], dataset2$n[i])
      } else if (i < 8) {
        dataset2$n[i] <- ifelse(dataset2$n[i] <= 0, mean(dataset2$n[c(i-1,i+1)]), dataset2$n[i])
      } else {
        dataset2$n[i] <- ifelse(dataset2$n[i] <= 0, mean(dataset2$n[(i-7):(i-1)]), dataset2$n[i])
      }
    }
    
    dataset2 <- dataset2 %>%
      mutate(sun = 1*I(weekdays(date) == "domingo"),
             mon = 1*I(weekdays(date) == "segunda")) %>%
      select(-date)
    
    fit2 <- gamlss(log(n) ~ cs(days)+sun+mon, data = dataset2, family = LO)
    nhat2 <- exp(fit2$mu.fv)
    # coefs = fit$mu.coefficients
    aux = seq((length(dataset2$days)+1),(length(dataset2$days)+input$janela), 1)
    future = aux
    future.days = weekdays(seq(df2$date[length(df2$date)]+1,
                               df2$date[length(df2$date)]+input$janela, 1))
    sun = 1*I(future.days == "domingo")
    mon = 1*I(future.days == "segunda")
    
    dataset2 <<- dataset2
    
    predn2 <- as.numeric(predict(
      fit2,
      what = "mu",
      newdata = data.frame(days = future,
                           sun = sun,
                           mon = mon),
      type = "response"
    ))
    
    sqr <- sum(fit2$residuals^2)
    nvar <- ncol(dataset2)-2
    nobs <- nrow(dataset2)
    quant <- qt(.975, nobs-nvar-1)
    mobs <- as.matrix(cbind(1, dataset2[,-c(1,2)]))
    mfut <- as.matrix(cbind(1, future, sun, mon))
    prodx <- t(mobs)%*%mobs
    w <- ginv(prodx)
    v <- c()
    for (i in 1:nrow(mfut)) {
      v[i] <- t(mfut[i,])%*%w%*%mfut[i,]
    }
    merror <- quant*sqrt((1/nobs)*sqr*v)
    l2 <- exp(predn2-merror)
    u2 <- exp(predn2+merror)
    
    dataset2 <<- data.frame(dataset2, nhat2)
    new.dataset2 <<- data.frame(predn2 = exp(predn2), days = future, l = l2, u = u2,
                                date = seq(df2$date[length(df2$date)]+1,
                                           df2$date[length(df2$date)]+input$janela, 1))
    last.day2 <- last(dataset2$days)
    
    plt2 <- ggplot() +
      geom_line(data = dataset2, aes(x = days, y = nhat2, color = "fitted", linetype = "fitted"),
                size = .75) +
      geom_point(data = dataset2, aes(x = days, y = n0), color = "red4") +
      geom_line(data = dataset2, aes(x = days, y = n0, color = "obs", linetype = "obs")) +
      geom_line(data = new.dataset2, aes(x = days, y = predn2, color = "predicted",
                                        linetype = "predicted"), size = .75) +
      geom_vline(xintercept = last.day2, color = "darkgrey") +
      geom_ribbon(data = new.dataset2, aes(x = days, ymin = l, ymax = u, fill = "cband"),
                  alpha = .3) +
      xlim(0, 500) +
      scale_color_manual(
        name = "",
        values = c("fitted" = "#D55E00",
                   "obs" = "red4",
                   "predicted" = "darkblue"),
        labels = c("Valores Ajustados",
                   "Valores Observados",
                   "Valores Preditos")
      ) +
      scale_fill_manual(
        name = "",
        values = c("cband" = "grey40"),
        labels = c("IC 95%")
      ) +
      scale_linetype_manual(
        name = "",
        values = c("fitted" = "solid",
                   "obs" = "solid",
                   "predicted" = "dashed"),
        labels = c("Valores Ajustados",
                   "Valores Observados",
                   "Valores Preditos")
      ) +
      labs(
        x = ifelse(input$metrics == "Novos Casos",
                   "Dias desde o 100º caso",
                   "Dias desde a 50ª morte"),
        y = paste("Número de", input$metrics),
        title = paste0(t1, ", ", capital(), " / ", input$uf),
        subtitle = paste0("Atualizado em ", format(hoje, "%d/%m/%Y")),
        caption = "Fonte: http://obsrpb.com.br/ufpb/"
      ) +
      theme_bw() +
      theme(legend.position = c(.9, .8),
            legend.title = element_blank(),
            legend.background = element_blank(),
            legend.spacing = unit(-3.15, "cm"))
    
    plt2
  })
  
  # output$tab1 <- renderDataTable({
  #   tab <- data.frame(format(new.dataset2$date, "%d/%m/%Y"), round(new.dataset$predn, 2),
  #                     round(new.dataset$l, 2), round(new.dataset$u, 2))
  #   tab <- DT::datatable(
  #     tab,
  #     colnames = c("Data", "Previsões", "Quantil 2,5%", "Quantil 97,5%"),
  #     rownames = FALSE,
  #     caption = paste0("Previsões de ", input$metrics, ", ", input$uf),
  #     extensions = "Buttons",
  #     options = list(
  #       dom = "Bt",
  #       pageLength = 30,
  #       fixedColumns = TRUE,
  #       autoWidth = TRUE,
  #       language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json'),
  #       buttons = list(buttons = c('csv', 'excel'))
  #     )
  #   )
  #   expr = tab
  # })
  # 
  # output$tab2 <- renderDataTable({
  #   tab <- data.frame(format(new.dataset2$date, "%d/%m/%Y"), round(new.dataset2$predn, 2),
  #                     round(new.dataset2$l, 2), round(new.dataset2$u, 2))
  #   tab <- DT::datatable(
  #     tab,
  #     colnames = c("Data", "Previsões", "Quantil 2,5%", "Quantil 97,5%"),
  #     rownames = FALSE,
  #     caption = paste0("Previsões de ", input$metrics, " para ", capital(), " / ", input$uf),
  #     extensions = "Buttons",
  #     options = list(
  #       dom = "Bt",
  #       pageLength = 30,
  #       fixedColumns = TRUE,
  #       autoWidth = TRUE,
  #       language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json'),
  #       buttons = list(buttons = c('csv', 'excel'))
  #     )
  #   )
  #   expr = tab
  # })
}
