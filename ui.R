fluidPage(
  titlePanel("Covid-19: Previsão de Curto Prazo para Estados e Capitais"),
  withMathJax(),
  # tags$p("O número reprodutivo basal ou razão de reprodução básica \\(R_0\\) indica o quão contagiosa uma doença infecciosa é. O número aponta quantas pessoas, em média, um indivíduo infeccioso pode contagiar em uma população totalmente suscetível. Apesar de ser muito útil para avaliar o potencial de propagação de doenças infecciosas em diferentes contextos, é uma medida teórica."),
  # tags$p("No momento em que as doenças infecciosas se propagam e indivíduos que já foram infectados tornam-se resistentes, não pertencendo mais ao grupo de suscetíveis, a premissa de uma população totalmente suscetível passa a não ser mais uma boa aproximação da realidade e uma nova medida epidemiológica faz-se necessária."),
  # tags$p("O número reprodutivo efetivo ou razão de reprodução efetiva \\(R_t\\) indica quantas pessoas, em média, um indivíduo infeccioso pode contagiar em uma população na qual nem todos são suscetíveis. Se \\(R_t<1\\), ou seja, se cada indivíduo infeccioso causa, em média, menos do que uma nova infecção, então os níveis de contágio da doença irão decair e a doença irá, eventualmente, desaparecer. Se \\(R_t=1\\), ou seja, se cada indivíduo infeccioso causa, em média, exatamente uma nova infecção, então os níveis de contágio da doença permanecerão etáveis e a doença se tornará endêmica. Se \\(R_t>1\\), ou seja, se cada indivíduo infeccioso causa, em média, mais do que uma nova infecção, então a doença se propagará na população e poderá haver uma epidemia."),
  tags$p("Nesta aplicação, apresentamos a previsões para o número de novos casos e de novas mortes por Covid-19 para os estados brasileiros e suas capitais. A metodologia utilizada encontra-se descrita no ", tags$a("Relatório Técnico", href = "http://obsrpb.com.br/ufpb/wp-content/uploads/2020/08/Relatorio_COVID19_03_OBSRUFPB.pdf"), "elaborado pelo ", tags$a("Observatório de Síndromes Respiratórias da UFPB", href = "http://obsrpb.com.br/ufpb/", .noWS = "after"), ". Por padrão, o aplicativo mostra as previsões para os dados da Paraíba. O usuário pode especificar um estado para o qual deseja visualizar as previsões. Para estimativas sobre o número reprodutivo efetivo, \\(R_t\\), acesse nossa ", tags$a("aplicação", href = "https://obsrpb.shinyapps.io/rt_estim/", .noWS = "after"), "."),
  sidebarLayout(
    sidebarPanel(
      shinyWidgets::pickerInput(inputId = "uf", label = "Selecione o estado:",
                                choices = list(
                                  Norte = c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima","Tocantins"),
                                  Nordeste = c("Alagoas","Bahia","Ceará",
                                               "Maranhão","Paraíba","Pernambuco",
                                               "Piauí","Rio Grande do Norte","Sergipe"),
                                  `Centro-Oeste` = c("Distrito Federal","Goiás","Mato Grosso","Mato Grosso do Sul"),
                                  Sudeste = c("Espírito Santo","Minas Gerais","Rio de Janeiro","São Paulo"),
                                  Sul = c("Paraná","Rio Grande do Sul","Santa Catarina")
                                ),
                                selected = "Paraíba"),
      radioButtons(
        inputId = "metrics",
        label = "Selecione o tipo de dado:", 
        choices = c("Novos Casos", "Novas Mortes"),
        selected = "Novos Casos",
        inline = FALSE
      ),
      sliderInput(
        inputId = "janela",
        label = "Quantidade de dias a serem preditos:",
        min = 7,
        max = 30,
        step = 1,
        value = 14
      ),
      p("Aplicativo desenvolvido pelo", a("Observatório de Síndromes Respiratórias da UFPB", href = "http://obsrpb.com.br/ufpb/", .noWS = "after"), ". Os dados utilizados foram obtidos do Repositório de Dados Públicos",
        a(href = "https://brasil.io", "Brasil.IO", .noWS = "after"), "."),
      p(a(img(src = "logo.png", height = "150px", style = "margin-top: 10px; margin-left: 20px"), href = "http://obsrpb.com.br/ufpb/"))
    ),
    mainPanel(
      # plotOutput("plot1")
      tabsetPanel(
        type = "tabs",
        selected = "Gráfico por Estado",

        # Aba com o gráfico das previsões por estado
        tabPanel("Gráfico por Estado",
                 plotOutput("plot1")
        ),

        # Aba com o gráfico das previsões por capital
        tabPanel("Gráfico por Capital",
                 plotOutput("plot2")
        )#,
        
        # Aba com uma tabela das previsões por estado
        # tabPanel("Tabela por Estado",
        #          DT::dataTableOutput("tab1")
        # ),
        
        # Aba com uma tabela das previsões por capital
        # tabPanel("Tabela por Capital",
        #          DT::dataTableOutput("tab2")
        # )
      )
    )
  )
)

