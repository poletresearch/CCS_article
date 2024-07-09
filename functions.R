#Code for the article Kazlou T., Cherp A., Jewell J.
#Feasible deployment of carbon capture and storage and the requirements of climate targets

#This is the code used for fitting growth curves to technology deployment timeseries. 
#Originally developed in Cherp et al. 2021. Available here: https://github.com/poletresearch/RES_article

fit_curve <- function(dt, fit = c("S", "G", "L", "E", "G_new"), t_exp = NA) {
  print(fit)  
  max.year <- max(dt$Year)
    min.year <- min(dt$Year)
    max.value <- max(dt$Value)
    t.exp <- ifelse(is.na(t_exp), min.year, t_exp)
    dt1 <- dt %>% mutate(Value0 = lag(Value), Delta = Value - Value0) %>%
      na.omit %>%
      filter(Delta == max(Delta)) #maximum annual change in values to locate the inflection point
    max.delta <- dt1$Year[1]  #inflection point year
    
    result <- data.frame()
    if ("S" %in% fit) {
      #Logistic fit
      xt <- max.delta
      catch = tryCatch({
        n1 <- nlsLM(Value ~ asym/(1 +  exp((xtime - Year) * k)),
                          start=list(asym = max.value, xtime = xt, k = 0.5), data = dt,
                          control = nls.lm.control(maxiter = 500))
              
        #Parsing logistic results
        asym <- coef(n1)["asym"]
        xtime <- coef(n1)["xtime"]
        k <- coef(n1)["k"]
        maturity <- 1/(1 +  exp((xtime - max.year) * k))
        g = asym * k/4
        rss <- n1$m$deviance()
        res <- data.frame(Fit = "S", K = k,  L = asym, TMax = xtime,
                          G = g, Maturity = maturity, RSS = rss, Good = 1)
      } , error = function(e) {
        print(str_c('Error!', " L"))
        print(e)
        res <- data.frame(Fit = "S", K = 0,  L = 0, TMax = 0,
                          G = 0, Maturity = 0, RSS = 0, Good = 0)
        return(res)
      }, finally = {
        
      }
      )
      res <- catch
      result <- result %>% rbind(res)
    } 
    
    if ("G" %in% fit) {
      xt <- max.delta
      #Gompertz fit
      catch = tryCatch({
        n2 <- nlsLM(Value ~ asym * exp(-  exp((xtime - Year) * k)),
                    start=list(asym = max.value, xtime = xt, k = 0.5), data = dt,
                    control = nls.lm.control(maxiter = 1000))
        
        #Parsing Gompertz results   
        asym <- coef(n2)["asym"]
        k <- coef(n2)["k"]
        xtime <- coef(n2)["xtime"]
        g = asym * k / exp(1)
        rss <- n2$m$deviance()
        maturity <- exp(- exp(k * (xtime - max.year)))
        res <- data.frame(Fit = "G", K = k,  L = asym, TMax = xtime, 
                          G = g, Maturity = maturity, RSS = rss, Good = 1)
      } , error = function(e) {
        print(str_c('Error!', " G"))
        print(e)
        res <- data.frame(Fit = "G", K = 0,  L = 0, TMax = 0,
                          G = 0, Maturity = 0, RSS = 0, Good = 0)
        return(res)
      }, finally = {
        
      }
      )
      res <- catch
      result <- result %>% rbind(res)
    } 
    
    if ("G_new" %in% fit) {
      xt <- max.delta
      #Gompertz fit
      catch = tryCatch({
        n0 <- nlsLM(Value ~ asym * exp(-  exp((xtime - Year) * k)),
                    start=list(asym = max.value * 3, xtime = xt, k = 0.5), data = dt,
                    control = nls.lm.control(maxiter = 1000))
        
        #Parsing Gompertz results   
        asym <- coef(n0)["asym"]
        k <- coef(n0)["k"]
        xtime <- coef(n0)["xtime"]
        g = asym * k / exp(1)
        rss <- n0$m$deviance()
        maturity <- exp(- exp(k * (xtime - max.year)))
        res <- data.frame(Fit = "G_new", K = k,  L = asym, TMax = xtime, 
                          G = g, Maturity = maturity, RSS = rss, Good = 1)
      } , error = function(e) {
        print(str_c('Error!', " G_new"))
        print(e)
        res <- data.frame(Fit = "G_new", K = 0,  L = 0, TMax = 0,
                          G = 0, Maturity = 0, RSS = 0, Good = 0)
        return(res)
      }, finally = {
        
      }
      )
      res <- catch
      result <- result %>% rbind(res)
    } 
    
    if ("L" %in% fit) {
      #Logistic-linear fit
      xt <- max.delta
      catch = tryCatch({
        n3 <- nlsLM(Value ~ ifelse(Year <= xtime, asym/(1 +  exp((xtime - Year) * k)),
                                   asym/2 + asym * k /4 * (Year - xtime)),
                    start=list(asym = max.value, xtime = xt, k = 0.5), data = dt,
                    control = nls.lm.control(maxiter = 500))
        
        #Parsing logistic results
        asym <- coef(n3)["asym"]
        xtime <- coef(n3)["xtime"]
        k <- coef(n3)["k"]
        maturity <- 1/(1 +  exp((xtime - max.year) * k))
        g = asym * k/4
        rss <- n3$m$deviance()
        res <- data.frame(Fit = "L", K = k,  L = asym, TMax = xtime,
                        G = g, Maturity = 0, RSS = rss, Good = 1)
      } , error = function(e) {
        print(str_c('Error!', " L"))
        print(e)
        res <- data.frame(Fit = "L", K = 0,  L = 0, TMax = 0,
                          G = 0, Maturity = 0, RSS = 0, Good = 0)
        return(res)
      }, finally = {
        
      }
      )
      res <- catch
      result <- result %>% rbind(res)
    }
    if ("E" %in% fit) {
      #Logistic fit
      catch = tryCatch({
        n4 <- nlsLM(Value ~ asym * exp((Year - t.exp) * k),
                    start=list(asym = max.value, k = 0.5), data = dt,
                    control = nls.lm.control(maxiter = 500))
        
        #Parsing logistic results
        asym <- coef(n4)["asym"]
        xtime <- coef(n4)["xtime"]
        k <- coef(n4)["k"]
        rss <- n4$m$deviance()
        res <- data.frame(Fit = "E", K = k,  L = asym, TMax = t.exp,
                          G = 0, Maturity = 0, RSS = rss, Good = 1)
      } , error = function(e) {
        print(str_c('Error!', " L"))
        print(e)
        res <- data.frame(Fit = "L", K = 0,  L = 0, TMax = 0,
                          G = 0, Maturity = 0, RSS = 0, Good = 0)
        return(res)
      }, finally = {
        
      }
      )
      res <- catch
      result <- result %>% rbind(res)
    } 
    return(result)
}