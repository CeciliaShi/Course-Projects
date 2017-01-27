#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(parallel)

# generate prior samples
shinyServer(function(input, output) {
  prior_samps = reactive({
    
    return(cbind(
      rnbinom(input$n_sims, mu = input$mu, size = -input$mu^2 / (input$mu - input$sd^2)),
      rbeta(input$n_sims, input$beta_alpha, input$beta_beta)))
    
  })
  
  # Generate simulated data
  sim_data = reactive(
    {
      gen_model = function(n_socks, prop)
      {
        n_pairs <- round(floor(n_socks / 2) * prop)
        n_odd <- n_socks - n_pairs * 2
        
        socks <- rep(seq_len(n_pairs + n_odd),
                     rep(c(2, 1), c(n_pairs, n_odd)))
        n_picked <- input$data_n_sin+2*input$data_n_pairs
        
        picked_socks <- sample(socks, size = min(n_picked, n_socks))
        sock_counts <- table(picked_socks)
        
        # returning the parameters and counts of the number of matched 
        # and unique socks among those that were picked out
        c(unique = sum(sock_counts == 1),
          pairs = sum(sock_counts == 2),
          n_socks = n_socks, n_pairs = n_pairs, 
          n_odd = n_odd, prop_pairs = prop)
      }
      
      sim_data = t(mclapply(1, function(x) apply(prior_samps(), 1, function(x) gen_model(x[1],x[2])), mc.cores=4)[[1]])
    })
  
  post_draws = reactive(
    { 
      sim_data()[sim_data()[, "unique"] == input$data_n_sin &
                   sim_data()[, "pairs" ] == input$data_n_pairs ,]
    })
  
  output$prior_plot = renderPlot(
    { 
      plot(density(sim_data()[,3]),main="Prior Number of Socks")
      if (input$showplot == TRUE){ # do not output the plot when the option is turned off
        abline(v=round(mean(sim_data()[,3])),col='red',lty=2)
        abline(v=round(median(sim_data()[,3])),col='blue',lty=2)
        legend("topright", c(paste('Mean: ',round(mean(sim_data()[,3]))),
                             paste('Median: ',round(median(sim_data()[,3]))) ),
               col=c('red','blue'), lty=c(2,2))
      }
    })
  
  output$prior_plot2 = renderPlot(
    { 
      plot(density(sim_data()[,4]),main="Prior Number of Pairs")
      if (input$showplot == TRUE){  # do not output the plot when the option is turned off
        abline(v=round(mean(sim_data()[,4])),col='red',lty=2)
        abline(v=round(median(sim_data()[,4])),col='blue',lty=2)
        legend("topright", c(paste('Mean: ',round(mean(sim_data()[,4]))),
                             paste('Median: ',round(median(sim_data()[,4]))) ),
               col=c('red','blue'), lty=c(2,2))
      }
    })
  
  output$posterior_plot = renderPlot(
    { 
      if(length(post_draws()) >0){  # do not output the summary when the option is turned off
        
        plot(density(post_draws()[,3]), main="Post. Number of Socks")
        if (input$showplot == TRUE){
          abline(v=round(mean(post_draws()[,3])),col='red',lty=2)
          abline(v=round(median(post_draws()[,3])),col='blue',lty=2)
          
          if (input$showtrue == TRUE){
            abline(v=45, lty=3)
            legend("topright", c(paste('Mean: ',round(mean(post_draws()[,3]))),
                                 paste('Median: ',round(median(post_draws()[,3]))),
                                 paste('True Value: 45')),
                   col=c('red','blue','black'), lty=c(2,2,3))
          } else{
            legend("topright", c(paste('Mean: ',round(mean(post_draws()[,3]))),
                                 paste('Median: ',round(median(post_draws()[,3]))) ),
                   col=c('red','blue'), lty=c(2,2))
          }
        }
      }
    })
  
  output$posterior_plot2 = renderPlot(
    { 
      if(length(post_draws()) >0){  # do not output the summary when the option is turned off
        
        plot(density(post_draws()[,4]), main="Post. Number of Pairs")
        if (input$showplot == TRUE){
          abline(v=round(mean(post_draws()[,4])),col='red',lty=2)
          abline(v=round(median(post_draws()[,4])),col='blue',lty=2)
          
          if (input$showtrue == TRUE){
            abline(v=21, lty=3)
            legend("topright", c(paste('Mean: ',round(mean(post_draws()[,4]))),
                                 paste('Median: ',round(median(post_draws()[,4]))),
                                 paste('True Value: 21')),
                   col=c('red','blue','black'), lty=c(2,2,3))
          } else{
            legend("topright", c(paste('Mean: ',round(mean(post_draws()[,4]))),
                                 paste('Median: ',round(median(post_draws()[,4]))) ),
                   col=c('red','blue'), lty=c(2,2))
          }
        }
      }
    })
  
  output$prior_summary = renderText(
    { 
      if(input$showtext == TRUE){
        str1 = paste("Mean Prior Number of Socks:",round(mean(sim_data()[,3])))
        str2 = paste("Median Prior Number of Socks:",round(median(sim_data()[,3])))
        paste(str1, str2, sep = "\n" )
      }
    })
  
  output$prior_summary2 = renderText(
    { 
      if(input$showtext == TRUE){
        str1 = paste("Mean Prior Number of Pairs:",round(mean(sim_data()[,4])))
        str2 = paste("Mean Prior Number of Singletons:",round(mean(sim_data()[,5])))
        str3 = paste("Median Prior Number of Pairs:",round(median(sim_data()[,4])))
        str4 = paste("Median Prior Number of Singletons:",round(median(sim_data()[,5])))
        paste(str1, str2, str3, str4, sep = "\n" )
      }
    })
  
  output$posterior_summary = renderText(
    { 
      if(length(post_draws()) >0 &input$showtext == TRUE){
        quant1 = paste(quantile(post_draws()[,3],c(0.025,0.975),names=FALSE), collapse = ", ")
        str1 = paste("Mean Posterior Number of Socks:",round(mean(post_draws()[,3])))
        str2 = paste("95% C.I.: [", quant1, "]")
        str3 = paste("Median Posterior Number of Socks:",round(median(post_draws()[,3])))
        paste(str1, str2, str3, sep = "\n" )
      }
      else if(length(post_draws()) == 0)
      {
        str4 = "No match was found based on the data and prior information"
        str4
      }
    })
  
  output$posterior_summary2 = renderText(
    { 
      if(length(post_draws()) >0 & input$showtext == TRUE){
        quant1 = paste(quantile(post_draws()[,4],c(0.025,0.975),names=FALSE), collapse = ", ")
        quant2 = paste(quantile(post_draws()[,5],c(0.025,0.975),names=FALSE), collapse = ", ")
        str1 = paste("Mean Posterior Number of Pairs:",round(mean(post_draws()[,4])))
        str2 = paste("95% C.I.: [", quant1, "]")
        str3 = paste("Mean Posterior Number of Singletons:",round(mean(post_draws()[,5])))
        str4 = paste("95% C.I.: [", quant2, "]")
        str5 = paste("Median Posterior Number of Pairs:",round(median(post_draws()[,4])))
        str6 = paste("Median Posterior Number of Singletons:",round(median(post_draws()[,5])))
        paste(str1, str2, str3, str4, str5, str6, sep = "\n")
      } 
      else if(length(post_draws()) == 0)
      {
        str7 = "No match was found based on the data and prior information"
        str7
      }
    })
  
  output$true_value = renderText(
    {
      if(input$showtrue ==TRUE){
        str1 = "There were 45 socks."
        str1
      }
    })
  
  output$true_value2 = renderText(
    {
      if(input$showtrue ==TRUE){
        str1 = "There were 21 pairs and 3 singletons."
        str1
      }
    })   
  
  
})
