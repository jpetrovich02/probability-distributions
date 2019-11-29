library(shiny)
library(ggplot2)
library(grid)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to update lambda.
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  lambda <- reactive({
    input$lambda
  })
  
  
  # Generate a plot of the distribution's pdf.
  # Uses the lambda input to update the pdf.
  output$explore <- renderPlot({
    lambda <- lambda()
    mu <- round(1/lambda,3)

    p0 <- ggplot(data = data.frame(x = c(0,10)), aes(x)) +
      stat_function(fun = dexp, n = 101, args = list(rate = lambda),color = "red",size = 2) +
      stat_function(fun = dexp, n = 101, args = list(rate = lambda),geom = 'area', fill = 'red', alpha = 0.2) +
      ggtitle(bquote("The Exponential Distribution with"~ lambda~ "="~ .(lambda)),
              subtitle = paste("Mean = ",mu, ", Standard Deviation = ",mu)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5,size = 24),
            plot.subtitle = element_text(hjust = 0.5,size = 18),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank())
    
    p0
  })
  
  
  # Generate a plot of the distribution's pdf shaded according to the selected probability.
  # User selects the type of shading (left tail, right tail, or interval) as well as
  # the probability (area) to be shaded. User also selects lambda, the rate parameter, which 
  # determines the shape of the pdf.
  output$probability <- renderPlot({
    lambda <- input$lambda2
    sig <- 1/lambda
    percentile <- input$x_quant
    mark <- textGrob(paste(percentile),gp = gpar(col = "orange2",fontsize = 20))
    
    # Determine the x-axis limits of the pdf plot
    xlim <- c(0,min(10,5*sig))
    
    p1 <- ggplot(data = data.frame(x = c(0,10)), aes(x)) +
      stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                    color = "red",size = 2,xlim = xlim) +
      scale_y_continuous(expand = expand_scale(mult = c(0,0.2))) +
      coord_cartesian(clip = "off") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5,size = 20),
            plot.subtitle = element_text(hjust = 0.5,size = 16),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank())
    
    if(input$prob == "lower"){
      lt.prob <- round(pexp(percentile,lambda,lower.tail = T),4)*100
      rt.prob <- round(100 - lt.prob,4)
      p1 <- p1 +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.2, xlim = xlim) +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.3, xlim = c(0,percentile)) +
        geom_segment(aes(x = percentile,xend = percentile,y = 0,yend = dexp(0,lambda)),
                     linetype = "dashed",size = 2) +
        annotate("text",x = percentile + 0.5, y = dexp(0,lambda),label = paste(rt.prob,"%",sep = ""),
                 size = 8,color = "red", fontface = 2, alpha = 0.2) +
        annotate("text",x = percentile - 0.5, y = dexp(0,lambda),label = paste(lt.prob,"%",sep = ""),
                 size = 8,color = "red",fontface = 2, alpha = 0.6) +
        annotate("text",x = percentile, y = 0, label = "X", color = "red",size = 6) +
        annotation_custom(mark,xmin=percentile,xmax=percentile,ymin=-0.07,ymax=-0.07) +
        ggtitle(bquote("The Exponential Distribution with"~ lambda~ "="~ .(lambda)),
                subtitle = bquote("P( X <"~ .(percentile) ~") =" ~ .(lt.prob) ~ "%"))
    }else if(input$prob == "upper"){
      rt.prob <- round(pexp(percentile,lambda,lower.tail = F),4)*100
      lt.prob <- round(100 - rt.prob,4)
      p1 <- p1 +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.2, xlim = xlim) +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.3, xlim = c(percentile,max(xlim))) +
        geom_segment(aes(x = percentile,xend = percentile,y = 0,yend = dexp(0,lambda)),
                     linetype = "dashed",size = 2) +
        annotate("text",x = percentile + 0.5, y = dexp(0,lambda),label = paste(rt.prob,"%",sep = ""),
                 size = 8,color = "red", fontface = 2, alpha = 0.6) +
        annotate("text",x = percentile - 0.5, y = dexp(0,lambda),label = paste(lt.prob,"%",sep = ""),
                 size = 8,color = "red",fontface = 2, alpha = 0.2) +
        annotate("text",x = percentile, y = 0, label = "X", color = "red",size = 6) +
        annotation_custom(mark,xmin=percentile,xmax=percentile,ymin=-0.07,ymax=-0.07) +
        ggtitle(bquote("The Exponential Distribution with"~ lambda~ "="~ .(lambda)),
                subtitle = bquote("P( X >"~ .(percentile) ~") =" ~ .(rt.prob) ~ "%"))
    }else if(input$prob == "interval"){
      a <- min(input$a,input$b)
      b <- max(input$a,input$b)
      interval.prob <- round(pexp(b,lambda,T) - pexp(a,lambda,T),4)*100
      lt.prob <- round(pexp(a,lambda,T),4)*100
      rt.prob <- round(pexp(b,lambda,F),4)*100
      mark.a <- textGrob(paste(a),gp = gpar(col = "orange2",fontsize = 20))
      mark.b <- textGrob(paste(b),gp = gpar(col = "orange2",fontsize = 20))
      
      p1 <- p1 +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.3, xlim = c(a,b)) +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.2, xlim = xlim) +
        geom_segment(aes(x = a,xend = a,y = 0,yend = dexp(0,lambda)),linetype = "dashed",size = 2) +
        annotate("text",x = b + 0.5, y = dexp(0,lambda),label = paste(rt.prob,"%",sep = ""),
                 size = 8,color = "red", fontface = 2, alpha = 0.2) +
        annotate("text",x = a - 0.5, y = dexp(0,lambda),label = paste(lt.prob,"%",sep = ""),
                 size = 8,color = "red", fontface = 2, alpha = 0.2) +
        annotate("text",x = a + (b - a)/2, y = dexp(0,lambda),label = paste(interval.prob,"%",sep = ""),
                 size = 8,color = "red", fontface = 2, alpha = 0.6) +
        geom_segment(aes(x = b,xend = b,y = 0,yend = dexp(0,lambda)),linetype = "dashed",size = 2) +
        annotate("text",x = a, y = 0, label = "X", color = "red",size = 6) +
        annotate("text",x = b, y = 0, label = "X", color = "red",size = 6) +
        annotation_custom(mark.a,xmin=a,xmax=a,ymin=-0.07,ymax=-0.07) +
        annotation_custom(mark.b,xmin=b,xmax=b,ymin=-0.07,ymax=-0.07) +
        ggtitle(bquote("The Exponential Distribution with"~ lambda~ "="~ .(lambda)),
                subtitle = bquote("P("~ .(a) ~"< X <"~ .(b) ~ ") = " ~ .(interval.prob) ~ "%"))
    }
    
    p1
  })
  
  
  # Generate a plot of the distribution's pdf shaded according to the selected probability.
  # User selects the type of shading (left tail or right tail) as well as
  # the percentile. User also selects lambda, the rate parameter, which 
  # determines the shape of the pdf.
  output$percentile <- renderPlot({
    lambda <- input$lambda3
    mu <- 1/lambda; sig <- mu
    
    # Determine the x-axis limits of the pdf plot
    xlim <- c(0,min(10,5*sig))
    
    p2 <- ggplot(data = data.frame(x = c(0,10)), aes(x)) +
      stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                    xlim = xlim, color = "red",size = 2) +
      stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                    geom = 'area', fill = 'red', alpha = 0.2) +
      scale_y_continuous(expand = expand_scale(mult = c(0,0.2))) +
      coord_cartesian(clip = "off") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5,size = 20),
            plot.subtitle = element_text(hjust = 0.5,size = 16),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank())
    
    if(input$quant == "lower"){
      x_prob <- as.numeric(input$x_prob_l)
      lt.prob <- round(x_prob,4)
      rt.prob <- round(100 - lt.prob,4)
      percentile <- round(qexp(lt.prob/100,lambda,T),4)
      mark <- textGrob(paste(percentile),gp = gpar(col = "orange2",fontsize = 20,fontface = "bold"))
      
      p2 <- p2 +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.3, xlim = c(0,percentile)) +
        geom_segment(aes(x = percentile,xend = percentile,y = 0,yend = dexp(0,lambda)),
                     linetype = "dashed",size = 2) +
        annotate("text",x = percentile + 0.5, y = dexp(0,lambda),label = paste(rt.prob,"%",sep = ""),
                 size = 8,color = "red", alpha = 0.2) +
        annotate("text",x = percentile - 0.5, y = dexp(0,lambda),label = paste(lt.prob,"%",sep = ""),
                 size = 8,color = "red",alpha = 0.6) +
        annotate("text",x = percentile, y = 0, label = "X", color = "red",size = 6) +
        annotation_custom(mark,xmin=percentile,xmax=percentile,ymin=-0.07,ymax=-0.07) +
        ggtitle(bquote("The Exponential Distribution with"~ lambda~ "="~ .(lambda)),
                subtitle = bquote("P( X <"~ .(percentile) ~") =" ~ .(lt.prob) ~ "%"))
    }else if(input$quant == "upper"){
      x_prob <- as.numeric(input$x_prob_u)
      rt.prob <- round(x_prob,4)
      lt.prob <- round(100 - rt.prob,4)
      percentile <- round(qexp(rt.prob/100,lambda,F),4)
      mark <- textGrob(paste(percentile),gp = gpar(col = "orange2",fontsize = 20,fontface = "bold"))
      
      p2 <- p2 +
        stat_function(fun = dexp, n = 101, args = list(rate = lambda),
                      geom = 'area', fill = 'red', alpha = 0.3, xlim = c(percentile,10)) +
        geom_segment(aes(x = percentile,xend = percentile,y = 0,yend = dexp(0,lambda)),
                     linetype = "dashed",size = 2) +
        annotate("text",x = percentile + 0.5, y = dexp(0,lambda),label = paste(rt.prob,"%",sep = ""),
                 size = 8,color = "red", alpha = 0.6) +
        annotate("text",x = percentile - 0.5, y = dexp(0,lambda),label = paste(lt.prob,"%",sep = ""),
                 size = 8,color = "red",alpha = 0.2) +
        annotate("text",x = percentile, y = 0, label = "X", color = "red",size = 6) +
        annotation_custom(mark,xmin=percentile,xmax=percentile,ymin=-0.07,ymax=-0.07) +
        ggtitle(bquote("The Exponential Distribution with"~ lambda~ "="~ .(lambda)),
                subtitle = bquote("P( X >"~ .(percentile) ~") =" ~ .(rt.prob) ~ "%"))
    }
    
    p2
  })
  
}