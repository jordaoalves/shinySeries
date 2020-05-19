### ui.R


dashboardPage(
  
  
  
  dashboardHeader(title  = "shinySeries"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Importando dados", tabName = "aba2", icon = icon("file")),
      menuItem("Gráfico de linha da série", tabName = "aba3", icon = icon("chart-line")),
      menuItem("Efeitos de sazonalidade", tabName = "aba4", icon = icon("chart-line")),
      menuItem("Gráfico de decomposição", tabName = "aba5", icon = icon("chart-line")),
      menuItem("Gráficos de tendência", tabName = "aba6", icon = icon("chart-line")),
      menuItem("Análise de estacionaridade", tabName = "aba7", icon = icon("chart-line")),
      menuItem("Transformações da série", tabName = "aba8", icon = icon("chart-line")),
      menuItem("Roteiro Modelo", tabName = "aba9", icon = icon("file-alt")),
      menuItem("Validação da série", tabName = "aba10", icon = icon("chart-line")),
      menuItem("Autocorrelações dos resíduos", tabName = "aba11", icon = icon("chart-line")),
      menuItem("Previsão ARIMA", tabName = "aba12", icon = icon("chart-line")),
      menuItem("Sobre", tabName = "aba13", icon = icon("info"))
      
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
#      tabItem(tabName = "aba1", 
#              fluidRow(
#                box(title = "Opções"),
#                box(title = "Resultado", status = "primary")
#              )
#      ),
      
      tabItem(tabName = "aba2",
              useShinyjs(),
              
              fluidRow(
                
                box(id="parametrosDadosEntrada", class="boxes",title = "Parâmetros da série:",
                    numericInput("anoInicial", label = "Ano inicial: ", value = 1998, min = 1000),
                    numericInput("anoFinal", label = "Ano final: ", value = 2019, min = 1000),
                    numericInput("frequencia", label = "Frequência: (ex: mensal = 12,  diária = 365...)", value = 12, min = 1),
                    numericInput("periodoInicial", label = "Período inicial (ex: Agosto = 8,  30/01 = 30...)" , value = 1, min = 1),
                   
                ),
                
                box(id="boxFile1", class="boxes",title = "Importando dados:",
                    fileInput("file1", "Selecione sua série:",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    ),
                    checkboxInput("header", "Header (arquivo com cabeçalho)", TRUE),
                    radioButtons("TYPEsep", NULL,
                                 c("csv (separador decimal com ponto)" = "read.csv",
                                   "csv2 (separador decimal com vírgula)" = "read.csv2" ),selected = "read.csv2"),
                   div(style="display:inline-block",downloadButton("downloadData", label = "Baixar modelo em formato csv",icon = icon("bar-chart-o"),style = "color: white;background-color: #56B4E9;")),
                    
                    
                    
                   # downloadButton("downloadData", label = " Baixar modelo de Série Temporal em formato CSV",icon = icon("bar-chart-o"))
                    
                   # )

                ),
                
               box(id="parametrosDadosEntradaAAA", class="boxes",title = "View:",
                   DTOutput('tbl')
                )
              )
      ),  
      
      
      tabItem(tabName = "aba3",
            fluidRow(
              box(title = "Gráfico de linha da série", plotOutput("plot1"),width = 9
                            )
                              )
                                ),
      tabItem(tabName = "aba4", 
              fluidRow(
                box(title = "Gráfico de tendência",
                    plotOutput("plot2")
                ),
                box(title = "Box-plot dos períodos",
                    plotOutput("plot3")
                )
              )        
      ),
      
      tabItem(tabName = "aba5", 
              fluidRow(
                box(title = "Gráfico de decomposição aditiva ou multipla",
                    plotOutput("plot4")),
                    box(title = "Ajuste do modelo de decomposição",
                        radioButtons("mD", ("Modelo de decomposição"),
                                     choices = list("Aditivo" = "add","Multiplicativo" = "mult")))
                    
                )
                     
      ),
      
      tabItem(tabName = "aba6", 
              fluidRow(
                box(title = "Gráfico utilizando o método da média móvel centrada",
                    plotOutput("plot5")
                    ),
                box(title = "Gráfico utilizando o método de Lowess",
                    plotOutput("plot6")
                    ),
                box(title = "Número de termos",
                    numericInput("termosM", label = "Termos de M: ", value = 7, min = 1)
                ),
                box(title = "Número de termos",
                    sliderInput("pLowess", ("Proporção: "),min = 0.01, max = 1, value = 0.5)
                    )
                      )
            ),
      
      tabItem(tabName = "aba7", 
              fluidRow(
                box(title = strong("Gráfico da série ( Zt ) versus Tempo (t)"),
                    plotOutput("plot7")
                ),
                box(title = strong("Gráfico da série ( Zt ) versus Tempo (t)"),
                    br(),
                    h4("➣ Se os dados apresentarem uma flutuação aleatória em torno de uma 
                            média constante, a série pode ser considerada estacionária;"),
                    br(),
                    h4("➣ Caso contrário, tentar uma transformação nos dados."),width = 4)
                
                      ),
              fluidRow(
                box(title = strong("Gráfico da função de autocorrelação amostral (FAC) 
                                   para série original Z1, Z2, Z3, ..., Zn"),
                    plotOutput("plot8")
                ),
                box(title = strong("Gráfico da função de autocorrelação amostral (FAC) 
                                   para série original Z1, Z2, Z3, ..., Zn"),
                            br(),
                            h4("➣ Se a FAC tiver um ponto de corte com decaimento rápido 
                            ou amortece-se abruptamente, a série pode ser considerada estacionária;"),
                            br(),
                            h4("➣ Se a FAC se amortecer lentamente, a série não poderá ser considerada 
                            estacionária; far-se-á necessário proceder a uma transformação nos dados."),
                            width = 4)
                      ),
              fluidRow(
                box(id="Dickey-Fuller", class="boxes",title = strong("Teste de Dickey-Fuller aumentado:"),
                    mainPanel(tableOutput("ADF")),
                    footer = h5(code("➣ Testa a hipótese nula de que existe raiz unitária,
                                ou seja, a série não é estacionária."))
                    ),
                box(id="KPSS", class="boxes",title = strong("Teste de KPSS:"),
                    mainPanel(tableOutput("KPSS")),
                    footer = h5(code("➣ Testa a hipótese nula de que não existe raiz unitária,
                                     ou seja, a série é estacionária."))
                    )
                ),
              fluidRow(
                box(id="resultado", class="boxes",title = strong("Resultado do Teste de estacionaridade:"),
                    mainPanel(tableOutput("resultado"))
                    )
                )
              ),
      tabItem(tabName = "aba8", 
              fluidRow(
                box(selectInput(inputId = "tipoTransfo",
                                "Caso queira utilizar uma pré-diferenciação na série modifique esta opção:",
                                choices = c("Zt (Série padrão)", 
                                            "Zt^1/2 (Raiz quadrada da série)",
                                            "Zt^1/4 (Raiz raiz quártica da série)",
                                            "ln(Zt) (Logaritmo natural da série)"
                                ))
                ),# end box
                box(selectInput(inputId = "tipoDiff",
                                "Caso queira tomar diferença na série modifique esta opção:",
                                choices = c("Nenhuma diferença", 
                                            "Uma diferença",
                                            "Duas diferenças")
                                )
                )# end box
              ),#end fluidRow
              
              fluidRow(
                box(title = strong("Gráfico da série ( Zt ) versus Tempo (t)"),
                    plotOutput("plot10")
                ),
                box(title = strong("Gráfico da série ( Zt ) versus Tempo (t)"),
                    br(),
                    h4("➣ Se os dados apresentarem uma flutuação aleatória em torno de uma 
                            média constante, a série pode ser considerada estacionária;"),
                    br(),
                    h4("➣ Caso contrário, tentar uma transformação nos dados."),width = 4)
                
              ),
              
              fluidRow(
                box(title = strong("Gráfico da função de autocorrelação amostral (FAC) 
                                   para série original Z1, Z2, Z3, ..., Zn"),
                    plotOutput("plot11")
                ),
                box(title = strong("Gráfico da função de autocorrelação amostral (FAC) 
                                   para série original Z1, Z2, Z3, ..., Zn"),
                    br(),
                    h4("➣ Se a FAC tiver um ponto de corte com decaimento rápido 
                            ou amortece-se abruptamente, a série pode ser considerada estacionária;"),
                    br(),
                    h4("➣ Se a FAC se amortecer lentamente, a série não poderá ser considerada 
                            estacionária; far-se-á necessário proceder a uma transformação nos dados."),
                    width = 4)
                
              ),
              
              fluidRow(
                box(title = strong("Gráfico da função de autocorrelação amostral (FAC) 
                                   para série original Z1, Z2, Z3, ..., Zn"),
                    plotOutput("plot12")
                ),
                box(title = strong("Gráfico da função de autocorrelação amostral (FAC) 
                                   para série original Z1, Z2, Z3, ..., Zn"),
                    br(),
                    h4("➣ Se a FAC tiver um ponto de corte com decaimento rápido 
                            ou amortece-se abruptamente, a série pode ser considerada estacionária;"),
                    br(),
                    h4("➣ Se a FAC se amortecer lentamente, a série não poderá ser considerada 
                            estacionária; far-se-á necessário proceder a uma transformação nos dados."),
                    width = 4)
              ),
              fluidRow(
                box(id="Dickey-Fuller", class="boxes",title = strong("Teste de Dickey-Fuller aumentado:"),
                    mainPanel(tableOutput("ADF2")),
                    footer = h5(code("➣ Testa a hipótese nula de que existe raiz unitária,
                                ou seja, a série não é estacionária."))
                ),
                box(id="KPSS", class="boxes",title = strong("Teste de KPSS:"),
                    mainPanel(tableOutput("KPSS2")),
                    footer = h5(code("➣ Testa a hipótese nula de que não existe raiz unitária,
                                     ou seja, a série é estacionária."))
                )
              ),
              fluidRow(
                box(id="resultado200", class="boxes",title = strong("Resultado do Teste de estacionaridade:"),
                    mainPanel(tableOutput("resultado2"))
                )
              )
      ),## end pag8
      
            tabItem(tabName = "aba9", 
                      fluidPage(
                        mainPanel(htmlOutput("showfile"), width = 12)
                      )
                    
                   
              
      ),## end pag9
      
      tabItem(tabName = "aba10", 
              fluidRow(
                box(title = "Número de termos do modelo ARIMA",
                    selectInput(inputId = "tipoTransfoABA10.1",
                                "Caso queira utilizar uma pré-diferenciação na série modifique esta opção:",
                                choices = c("Zt (Série padrão)", 
                                            "Zt^1/2 (Raiz quadrada da série)",
                                            "Zt^1/4 (Raiz raiz quártica da série)",
                                            "ln(Zt) (Logaritmo natural da série)"
                                )),
                    numericInput("AtermosAR", label = "Termos AR: ", value = 1, min = 0, max = 3),
                    numericInput("AtermosI", label = "Termos I: ", value = 1, min = 0, max = 2),
                    numericInput("AtermosMA", label = "Termos MA: ", value = 1, min = 0, max = 3)
                ),
                box(title = "Número de termos do modelo ARIMA sobrefixado",
                    selectInput(inputId = "tipoTransfoABA10.2",
                                "Caso queira utilizar uma pré-diferenciação na série modifique esta opção:",
                                choices = c("Zt (Série padrão)", 
                                            "Zt^1/2 (Raiz quadrada da série)",
                                            "Zt^1/4 (Raiz raiz quártica da série)",
                                            "ln(Zt) (Logaritmo natural da série)"
                                )),
                    numericInput("BtermosAR", label = "Termos AR: ", value = 2, min = 0, max = 3),
                    numericInput("BtermosI", label = "Termos I: ", value = 1, min = 0, max = 2),
                    numericInput("BtermosMA", label = "Termos MA: ", value = 1, min = 0, max = 3)
                )
              ),
              fluidRow(
                box(id="sub1", class="boxes",title = strong("Modelo ARIMA:"),
                    mainPanel(tableOutput("box1sob"))),
                
                box(id="sub2", class="boxes",title = strong("Modelo ARIMA sobrefixado:"),
                    mainPanel(tableOutput("box2sob")))
              ),
              fluidRow(
                box(title = strong("Gráfico da  série de resíduos"),
                    plotOutput("plot13")
                ),
                
                box(title = "Número de termos do modelo ARIMA",
                    selectInput(inputId = "tipoTransfoABA10.3",
                                "Caso queira utilizar uma pré-diferenciação na série modifique esta opção:",
                                choices = c("Zt (Série padrão)", 
                                            "Zt^1/2 (Raiz quadrada da série)",
                                            "Zt^1/4 (Raiz raiz quártica da série)",
                                            "ln(Zt) (Logaritmo natural da série)"
                                )),
                    numericInput("CtermosAR", label = "Termos AR: ", value = 2, min = 0, max = 3),
                    numericInput("CtermosI", label = "Termos I: ", value = 1, min = 0, max = 2),
                    numericInput("CtermosMA", label = "Termos AR: ", value = 1, min = 0, max = 3)
                )
              ),
              fluidRow(
                box(title = strong("FAC dos resíduos:"),
                    plotOutput("plot14")
                ),
                box(title = strong("FACP dos resíduos:"),
                    plotOutput("plot15")
                ),
                box(h4("➣ Teste de normalidade dos resíduos:"),
                    br(),
                    mainPanel(tableOutput("shapiro"))
                ),
                box(
                  h4("➣ Média dos resíduos:"),
                  br(),
                  mainPanel(tableOutput("media"))
                )
              )
              
      ),  ### end pag10 
      
      tabItem(tabName = "aba11", 
              fluidRow(
                box(title = "Número de termos do modelo ARIMA",
                    selectInput(inputId = "tipoTransfoABA11",
                                "Caso queira utilizar uma pré-diferenciação na série modifique esta opção:",
                                choices = c("Zt (Série padrão)", 
                                            "Zt^1/2 (Raiz quadrada da série)",
                                            "Zt^1/4 (Raiz raiz quártica da série)",
                                            "ln(Zt) (Logaritmo natural da série)"
                                )),
                    numericInput("DtermosAR", label = "Termos AR: ", value = 0, min = 0, max = 3),
                    numericInput("DtermosI", label = "Termos I: ", value = 0, min = 0, max = 2),
                    numericInput("DtermosMA", label = "Termos MA: ", value = 0, min = 0, max = 3),
                    br(),
                    numericInput("Kpassos", label = "K passos: ", value = 1, min = 1, max = 30)
                ),
                box(id="corres1", class="boxes",title = strong("Teste Box-Pierce:"),
                    mainPanel(tableOutput("boxpierce"))
                    
                ),
                box(id="corres1", class="boxes",title = strong("Teste Ljung-Box:"),
                    mainPanel(tableOutput("ljungbox"))
                    
                )
                
                
              )
              
      ), ## end pag11
      
      
      tabItem(tabName = "aba12", 
              fluidRow(
                box(title = strong("Gráfico de previsão da série"),
                   # plotOutput("plot16"),width = 9
                    plotlyOutput("plot16"),width = 9
                ),
                box(title = "Número de termos do modelo ARIMA",
                    selectInput(inputId = "tipoTransfoARIMA",
                                "Caso queira utilizar uma pré-diferenciação na série modifique esta opção:",
                                choices = c("Zt (Série padrão)", 
                                            "Zt^1/2 (Raiz quadrada da série)",
                                            "Zt^1/4 (Raiz raiz quártica da série)",
                                            "ln(Zt) (Logaritmo natural da série)"
                                )),
                    numericInput("EtermosAR", label = "Termos AR: ", value = 0, min = 0, max = 3),
                    numericInput("EtermosI", label = "Termos I: ", value = 0, min = 0, max = 2),
                    numericInput("EtermosMA", label = "Termos MA: ", value = 0, min = 0, max = 3),
                    br(),
                    numericInput("KPassosPrev", label = "K passos: ", value = 1, min = 1, max = 30),
                    width = 3
                )
              ),
              tableOutput("dadosPrev")
              
              
              
              
      ),## end pag12
      
      tabItem(tabName = "aba13", 
              fluidRow(
                       fluidPage(
                           column(7,
                                  h4("O aplicativo:"),
                                  p("Foi resultado de um trabalho para a disciplina DDA0103 - SÉRIES TEMPORAIS PARA CIÊNCIAS ATUARIAIS em 2019.1."),
                                  p("O aplicativo foi desenvolvido para facilitar o processo de análise e previsão de séries temporais. Com ele é possível inserir dados de séries temporais e obter desde uma simples visualização gráfica até uma previsão da série utilizando modelos ARIMA."),
                             #     p("Com ele é possível inserir dados de séries temporais e obter desde uma simples visualização gráfica até uma previsão da série utilizando modelos ARIMA."),
                                  br(),
                                  h4("Autor:"),
                                  p("Jordão de Lima Alves - Graduando em Ciências Atuariais pela UFRN."),
                                  p(a("www.jordaoalves.xyz", href = 'https://www.jordaoalves.xyz/' )),
                                  br(),
                                  h4("Agradecimentos:"),
                                  p("Marcos Roberto Gonzaga - DDCA/UFRN" ),
                                  p("Marcus Alexandre Nunes - DEST/UFRN" ),
                                  p(a("www.marcusnunes.me", href = 'https://marcusnunes.me/' )),
                                  br(),
                                  h4("Github:"),
                                  p(a("https://github.com/jordaoalves/shinySeries", href = 'https://github.com/jordaoalves/shinySeries' ))
                                  
                                  
                                  
                                  
                           )))
              
              
              
              
      )## end pag13
      
    ),busyIndicator()   
  )
)



