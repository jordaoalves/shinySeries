## server.R

function(input, output, session) {
  
 
   
   
   output$showfile <- renderUI({
      includeHTML("www/htmlFile.html")
      # HTML(readLines(file_to_show))
   })
   
   
   
   
   output$tbl = renderDT(
      
      if (is.null(input$file1) == FALSE){
         
         get(paste0(input$TYPEsep))(input$file1$datapath, header = input$header)
         
      }else{
         return(NULL)
      }
             )
   
   
   
   
   
   
   
   output$downloadData <- downloadHandler(
      filename <- function() {
         paste("cestas", "csv", sep=".")
      },
      
      content <- function(file) {
         file.copy("cestas.csv", file)
      },
      contentType = "cestas/csv"
   )
   
   
   
   
    output$plot1 <- renderPlot({
     
     inFile <- input$file1
     if (is.null(inFile))
     return(NULL)
     dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header) 
     dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
                    
     plot(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
          main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
     
     
   }) ## grafico da serie
   
   output$plot2 <- renderPlot({
     
     inFile <- input$file1
     if (is.null(inFile))
       return(NULL)
     dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
     dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
     
     plot(aggregate(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
          main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
     
     
   }) ## grafico de tendencia
  
   output$plot3 <- renderPlot({
     
     inFile <- input$file1
     if (is.null(inFile))
       return(NULL)
     dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
     dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
     
     boxplot(dados.ts~cycle(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
          main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
     
     
   }) ## box-plot
   
   output$plot4 <- renderPlot({
     
     inFile <- input$file1
     if (is.null(inFile))
       return(NULL)
     dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
     dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
     
     plot(decompose(dados.ts, type = input$mD))
     
     
     
   }) ## grafico de decomposição
   
   output$plot5 <- renderPlot({
     
     inFile <- input$file1
     if (is.null(inFile))
       return(NULL)
     dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
     dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
     
     tamanhoVetor <- 1:length(dados.ts)
     mediaMovel <- stats::filter(dados[2], sides = 2, rep(1,input$termosM)/input$termosM)
     
          plot(tamanhoVetor,dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
          main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
          lines(mediaMovel,col="dark orange")
     
   }) ## grafico mmc
   
   output$plot6 <- renderPlot({
     
     inFile <- input$file1
     if (is.null(inFile))
       return(NULL)
     dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
     dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
     
     plot(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",type="p",
          main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
          lines(lowess(dados.ts, f=input$pLowess), col = "blue")
     
     
    }) ## grafico de lowess
   
   #########################################################
   
   output$plot7 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      plot(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
           main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
      
      
   }) ## grafico da serie
   
   output$plot8 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      Acf(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
           main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
      
      
   }) ## grafico da acf
   
   output$plot9 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      Acf(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
          main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
      
      
   }) ## grafico da acf
   
   output$ADF <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      #ADF <- data.frame(adf.test(dados.ts)$p.value)
      #names(ADF) <- "p-valor"
      #ADF
      adf.test(dados.ts)
      
      
       
    }) ## teste ADF
   
   output$KPSS <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      #KPSS <- data.frame(kpss.test(dados.ts)$p.value)
      #names(KPSS) <- "p-valor"
      #KPSS
      kpss.test(dados.ts)
      
      
   }) ## teste KPSS
   
   output$resultado <- renderText({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      ADF <- data.frame(adf.test(dados.ts)$p.value)
      KPSS <- data.frame(kpss.test(dados.ts)$p.value)
      
      if(ADF < 0.05 && KPSS >= 0.05 ){
         
         print("Esta série é estacionária!")   
      }else{
         
         print("Esta série não é estacionária! Tente fazer alguma transformação nela.")
       }
      
      
   }) ## resultado estac.
   
   #####################################################
   
   output$plot10 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Nenhuma diferença") {
        
         plot(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
         main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
          
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         plot((dados.ts^(1/2)),ylab = colnames(dados.ts),xlab = "Tempo",
         main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         plot((dados.ts^(1/4)),ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         plot(log(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
         } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Uma diferença") {
            
            plot(diff(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
            
         } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Uma diferença") {
            
            plot(diff(dados.ts^(1/2)),ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
            
         } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Uma diferença") {
            
            plot(diff(dados.ts^(1/4)),ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
            
         } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Uma diferença") {
            
            plot(diff(log(dados.ts)),ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
            
         } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Duas diferenças") {
            
            plot(diff(diff(dados.ts)),ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
            
         } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Duas diferenças") {
            
            plot(diff(diff(dados.ts^(1/2))),ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
            
         } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Duas diferenças") {
            
            plot(diff(diff(dados.ts^(1/4))),ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
            
      } else {
         plot(diff(diff(log(dados.ts))),ylab = colnames(dados.ts),xlab = "Tempoooo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
      }
      
      
      
      
   }) ## grafico da serie
   
   output$plot11 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Nenhuma diferença") {
         
         Acf(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         Acf((dados.ts^(1/2)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         Acf((dados.ts^(1/4)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         Acf(log(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Uma diferença") {
         
         Acf(diff(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Uma diferença") {
         
         Acf(diff(dados.ts^(1/2)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Uma diferença") {
         
         Acf(diff(dados.ts^(1/4)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Uma diferença") {
         
         Acf(diff(log(dados.ts)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Duas diferenças") {
         
         Acf(diff(diff(dados.ts)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Duas diferenças") {
         
         Acf(diff(diff(dados.ts^(1/2))),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Duas diferenças") {
         
         Acf(diff(diff(dados.ts^(1/4))),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else {
         Acf(diff(diff(log(dados.ts))),ylab = colnames(dados.ts),xlab = "Tempoooo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
      }
      
      
      
   }) ## grafico da acf
   
   output$plot12 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Nenhuma diferença") {
         
         Pacf(dados.ts,ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         Pacf((dados.ts^(1/2)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         Pacf((dados.ts^(1/4)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         Pacf(log(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Uma diferença") {
         
         Pacf(diff(dados.ts),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Uma diferença") {
         
         Pacf(diff(dados.ts^(1/2)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Uma diferença") {
         
         Pacf(diff(dados.ts^(1/4)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Uma diferença") {
         
         Pacf(diff(log(dados.ts)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Duas diferenças") {
         
         Pacf(diff(diff(dados.ts)),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Duas diferenças") {
         
         Pacf(diff(diff(dados.ts^(1/2))),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Duas diferenças") {
         
         Pacf(diff(diff(dados.ts^(1/4))),ylab = colnames(dados.ts),xlab = "Tempo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
         
      } else {
         Pacf(diff(diff(log(dados.ts))),ylab = colnames(dados.ts),xlab = "Tempoooo",
             main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,sep = " "))
      }
      
      
      
   }) ## grafico da pacf
   
   output$ADF2 <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Nenhuma diferença") {
         
         adf.test(dados.ts)
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         adf.test((dados.ts^(1/2)))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         adf.test((dados.ts^(1/4)))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         adf.test(log(dados.ts))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Uma diferença") {
         
         adf.test(diff(dados.ts))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Uma diferença") {
         
         adf.test(diff(dados.ts^(1/2)))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Uma diferença") {
         
         adf.test(diff(dados.ts^(1/4)))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Uma diferença") {
         
         adf.test(diff(log(dados.ts)))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Duas diferenças") {
         
         adf.test(diff(diff(dados.ts)))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Duas diferenças") {
         
         adf.test(diff(diff(dados.ts^(1/2))))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Duas diferenças") {
         
         adf.test(diff(diff(dados.ts^(1/4))))
         
      } else {
         adf.test(diff(diff(log(dados.ts))))
      }

   }) ## teste ADF
   
   output$KPSS2 <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Nenhuma diferença") {
         
         kpss.test(dados.ts)
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         kpss.test((dados.ts^(1/2)))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         kpss.test((dados.ts^(1/4)))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         kpss.test(log(dados.ts))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Uma diferença") {
         
         kpss.test(diff(dados.ts))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Uma diferença") {
         
         kpss.test(diff(dados.ts^(1/2)))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Uma diferença") {
         
         kpss.test(diff(dados.ts^(1/4)))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Uma diferença") {
         
         kpss.test(diff(log(dados.ts)))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Duas diferenças") {
         
         kpss.test(diff(diff(dados.ts)))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Duas diferenças") {
         
         kpss.test(diff(diff(dados.ts^(1/2))))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Duas diferenças") {
         
         kpss.test(diff(diff(dados.ts^(1/4))))
         
      } else {
         kpss.test(diff(diff(log(dados.ts))))
      }
      
      
   }) ## teste KPSS
   
   output$resultado2 <- renderText({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Nenhuma diferença") {
         
         KPSS2 <- kpss.test(dados.ts)
         ADF2 <- adf.test(dados.ts)
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         KPSS2 <- kpss.test(dados.ts^(1/2))
         ADF2 <- adf.test(dados.ts^(1/2))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         KPSS2 <- kpss.test(dados.ts^(1/4))
         ADF2 <- adf.test(dados.ts^(1/4))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Nenhuma diferença") {
         
         KPSS2 <- kpss.test(log(dados.ts))
         ADF2 <- adf.test(log(dados.ts))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Uma diferença") {
         
         KPSS2 <- kpss.test(diff(dados.ts))
         ADF2 <- adf.test(diff(dados.ts))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Uma diferença") {
         
         KPSS2 <- kpss.test(diff(dados.ts^(1/2)))
         ADF2 <- adf.test(diff(dados.ts^(1/2)))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Uma diferença") {
         
         KPSS2 <- kpss.test(diff(dados.ts^(1/4)))
         ADF2 <- adf.test(diff(dados.ts^(1/4)))
         
      } else if (input$tipoTransfo == "ln(Zt) (Logaritmo natural da série)" && input$tipoDiff == "Uma diferença") {
         
         KPSS2 <- kpss.test(diff(log(dados.ts)))
         ADF2 <- adf.test(diff(log(dados.ts)))
         
      } else if (input$tipoTransfo == "Zt (Série padrão)" && input$tipoDiff == "Duas diferenças") {
         
         KPSS2 <- kpss.test(diff(diff(dados.ts)))
         ADF2 <- adf.test(diff(diff(dados.ts)))
         
      } else if (input$tipoTransfo == "Zt^1/2 (Raiz quadrada da série)" && input$tipoDiff == "Duas diferenças") {
         
         KPSS2 <- kpss.test(diff(diff(dados.ts^(1/2))))
         ADF2 <- adf.test(diff(diff(dados.ts^(1/2))))
         
      } else if (input$tipoTransfo == "Zt^1/4 (Raiz raiz quártica da série)" && input$tipoDiff == "Duas diferenças") {
         
         KPSS2 <- kpss.test(diff(diff(dados.ts^(1/4))))
         ADF2 <- adf.test(diff(diff(dados.ts^(1/4))))
         
      } else {
         KPSS2 <- kpss.test(diff(diff(log(dados.ts))))
         ADF2 <- adf.test(diff(diff(log(dados.ts))))
      }
      
      if(data.frame(ADF2$p.value) < 0.05 && data.frame(KPSS2$p.value) >= 0.05 ){
         
         print("Esta série é estacionária!")   
      }else{
         
         print("Esta série não é estacionária! Tente fazer alguma transformação nela.")
      }
      
      
   }) ## resultado estac.
   
   
   #############################################################################
   
   
   
   output$box1sob <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
     sob1 <- if (input$tipoTransfoABA10.1 == "Zt (Série padrão)") {
         
         print(arima(dados.ts, order = c(input$AtermosAR,input$AtermosI,input$AtermosMA)))
         
      } else if (input$tipoTransfoABA10.1 == "Zt^1/2 (Raiz quadrada da série)") {
         
         print(arima((dados.ts^(1/2)), order = c(input$AtermosAR,input$AtermosI,input$AtermosMA)))
         
      } else if (input$tipoTransfoABA10.1 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         print(arima((dados.ts^(1/4)), order = c(input$AtermosAR,input$AtermosI,input$AtermosMA)))
         
      } else {
         print(arima(log(dados.ts), order = c(input$AtermosAR,input$AtermosI,input$AtermosMA)))
      }
      
   }) ## teste sobreposição aba1
   
   output$box2sob <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      sob2 <- if (input$tipoTransfoABA10.2 == "Zt (Série padrão)") {
         
         print(arima(dados.ts, order = c(input$BtermosAR,input$BtermosI,input$BtermosMA)))
         
      } else if (input$tipoTransfoABA10.2 == "Zt^1/2 (Raiz quadrada da série)") {
         
         print(arima((dados.ts^(1/2)), order = c(input$BtermosAR,input$BtermosI,input$BtermosMA)))
         
      } else if (input$tipoTransfoABA10.2 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         print(arima((dados.ts^(1/4)), order = c(input$BtermosAR,input$BtermosI,input$BtermosMA)))
         
      } else {
         print(arima(log(dados.ts), order = c(input$BtermosAR,input$BtermosI,input$BtermosMA)))
      }
      
   }) ## teste sobreposição aba2
  
   output$plot13 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      plote13 <- if (input$tipoTransfoABA10.3 == "Zt (Série padrão)") {
         
         plot(arima(dados.ts, order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
             ylab = colnames(dados.ts),xlab = "Tempo",
                 main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                     sep = " "))
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/2 (Raiz quadrada da série)") {
         
         plot(arima((dados.ts^(1/2)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         plot(arima((dados.ts^(1/4)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
         ylab = colnames(dados.ts),xlab = "Tempo",
         main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                      sep = " "))
         
      } else {
         plot(arima(log(dados.ts), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
         ylab = colnames(dados.ts),xlab = "Tempo",
         main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                      sep = " "))
      }
      
      
   }) ## grafico do modelo arima
   
   output$plot14 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      plote13 <- if (input$tipoTransfoABA10.3 == "Zt (Série padrão)") {
         
         Acf(arima(dados.ts, order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/2 (Raiz quadrada da série)") {
         
         Acf(arima((dados.ts^(1/2)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         Acf(arima((dados.ts^(1/4)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
      } else {
         
         Acf(arima(log(dados.ts), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
      
         
         }
      
      
   }) ## grafico da acf do modelo arima
   
   output$plot15 <- renderPlot({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      plote14 <- if (input$tipoTransfoABA10.3 == "Zt (Série padrão)") {
         
         Pacf(arima(dados.ts, order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
      
         } else if (input$tipoTransfoABA10.3 == "Zt^1/2 (Raiz quadrada da série)") {
         
         Pacf(arima((dados.ts^(1/2)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
         
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         Pacf(arima((dados.ts^(1/4)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
      } else {
         
         Pacf(arima(log(dados.ts), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals,
              ylab = colnames(dados.ts),xlab = "Tempo",
              main = paste(colnames(dados.ts),"de",input$anoInicial,"à",input$anoFinal,
                           sep = " "))
         
         
      }
      
      
   }) ## grafico da pacf do modelo arima
   
   output$shapiro <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      shapirow <- if (input$tipoTransfoABA10.3 == "Zt (Série padrão)") {
         
         shapiro.test(arima(dados.ts, order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/2 (Raiz quadrada da série)") {
         
         shapiro.test(arima((dados.ts^(1/2)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         shapiro.test(arima((dados.ts^(1/4)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
      } else {
         
         shapiro.test(arima(log(dados.ts), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
         
      }
      
      print(shapirow)
      
      
   }) ## teste de normalidade
   
   output$media <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      mediaa <- if (input$tipoTransfoABA10.3 == "Zt (Série padrão)") {
         
         mean(arima(dados.ts, order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/2 (Raiz quadrada da série)") {
         
         mean(arima((dados.ts^(1/2)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
      } else if (input$tipoTransfoABA10.3 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         mean(arima((dados.ts^(1/4)), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
      } else {
         
         mean(arima(log(dados.ts), order = c(input$CtermosAR,input$CtermosI,input$CtermosMA))$residuals)
         
         
      }
      
      print(mediaa)
      
      
   }) ## media da série
   
   
   
   #######################################################################################
   
   
   output$boxpierce <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      boxp <- if (input$tipoTransfoABA11 == "Zt (Série padrão)") {
         
         Box.test(arima(dados.ts, order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Box-Pierce")
         
      } else if (input$tipoTransfoABA11 == "Zt^1/2 (Raiz quadrada da série)") {
         
         Box.test(arima((dados.ts^(1/2)), order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Box-Pierce")
         
      } else if (input$tipoTransfoABA11 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         Box.test(arima((dados.ts^(1/4)), order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Box-Pierce")
         
      } else {
         
         Box.test(arima(log(dados.ts), order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Box-Pierce")
         
         
      }
      
      print(boxp)
      
      
   }) ## teste box-piece
   
   output$ljungbox <- renderPrint({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      ljungb <- if (input$tipoTransfoABA11 == "Zt (Série padrão)") {
         
         Box.test(arima(dados.ts, order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Ljung-Box")
         
      } else if (input$tipoTransfoABA11 == "Zt^1/2 (Raiz quadrada da série)") {
         
         Box.test(arima((dados.ts^(1/2)), order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Ljung-Box")
         
      } else if (input$tipoTransfoABA11 == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         Box.test(arima((dados.ts^(1/4)), order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Ljung-Box")
         
      } else {
         
         Box.test(arima(log(dados.ts), order = c(input$DtermosAR,input$DtermosI,input$DtermosMA))$residuals,
                  lag = input$Kpassos, type = "Ljung-Box")
         
         
      }
      
      print(ljungb)
      
      
   }) ## media da ljung box
   
   output$plot16 <- renderPlotly({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      plote16 <- if (input$tipoTransfoARIMA == "Zt (Série padrão)") {
         
        prev <- forecast(arima(dados.ts, order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
         p <- autoplotly(prev)
         
         # You can apply additional ggplot2 elements to the generated interactive plot
         p + ggplot2::ggtitle(paste(colnames(dados.ts),"ARIMA","(",input$EtermosAR,input$EtermosI,input$EtermosMA,")",sep = " ")) +
          
            xlab("Tempo") + ylab(colnames(dados.ts)) 
         
         
         
      } else if (input$tipoTransfoARIMA == "Zt^1/2 (Raiz quadrada da série)") {
         
         prev <- forecast(arima((dados.ts^(1/2)) , order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
         p <- autoplotly(prev)
         
         # You can apply additional ggplot2 elements to the generated interactive plot
         p + ggplot2::ggtitle(paste(colnames(dados.ts),"ARIMA","(",input$EtermosAR,input$EtermosI,input$EtermosMA,")",sep = " ")) +
            
            xlab("Tempo") + ylab(colnames(dados.ts)) 
         
         
         
      } else if (input$tipoTransfoARIMA == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         prev <- forecast(arima((dados.ts^(1/4)) , order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
         p <- autoplotly(prev)
         
         # You can apply additional ggplot2 elements to the generated interactive plot
         p + ggplot2::ggtitle(paste(colnames(dados.ts),"ARIMA","(",input$EtermosAR,input$EtermosI,input$EtermosMA,")",sep = " ")) +
            
            xlab("Tempo") + ylab(colnames(dados.ts)) 
         
         
      } else {
         
         prev <- forecast(arima(log(dados.ts) , order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
         p <- autoplotly(prev)
         
         # You can apply additional ggplot2 elements to the generated interactive plot
         p + ggplot2::ggtitle(paste(colnames(dados.ts),"ARIMA","(",input$EtermosAR,input$EtermosI,input$EtermosMA,")",sep = " ")) +
            
            xlab("Tempo") + ylab(colnames(dados.ts)) 
         
         
      }
      
      
   }) ## grafico da Previsão
   
   output$dadosPrev <- renderTable({
      
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      dados <- get(paste0(input$TYPEsep))(inFile$datapath, header = input$header)  
      dados.ts <- ts(dados[2], start = c(input$anoInicial, input$periodoInicial), freq = input$frequencia)
      
      plote16 <- if (input$tipoTransfoARIMA == "Zt (Série padrão)") {
         
      forecast(arima(dados.ts, order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
      } else if (input$tipoTransfoARIMA == "Zt^1/2 (Raiz quadrada da série)") {
         
         forecast(arima((dados.ts^(1/2)) , order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
         
      } else if (input$tipoTransfoARIMA == "Zt^1/4 (Raiz raiz quártica da série)") {
         
         forecast(arima((dados.ts^(1/4)) , order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
         
      } else {
         
         forecast(arima(log(dados.ts) , order = c(input$EtermosAR,input$EtermosI,input$EtermosMA)),h=input$KPassosPrev)
         
         
      }
      
      
   }) ## dados da previsao
   
   
   
   
}