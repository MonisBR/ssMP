ssMPClass <- R6::R6Class(
  "ssMPClass",
  inherit = ssMPBase,
  private = list(
    
    .run = function() {
      
      tipo <- self$options$tipo
      
      # --- 1. ONE MEAN ---
      if (tipo == "media") {
        
        S <- self$options$S
        e_OM <- self$options$e_OM
        level_OM <- self$options$level_OM
        
        alpha <- 1 - level_OM
        z <- qnorm(1 - alpha / 2)
        n <- ceiling((z * S / e_OM)^2)
        
        table <- self$results$tableOM
        
        table$setRow(rowNo = 1, values = list(parameter = "Sample size", value = as.character(n)))
        table$setRow(rowNo = 2, values = list(parameter = "Precision", value = as.character(e_OM)))
        table$setRow(rowNo = 3, values = list(parameter = "Standard deviation", value = as.character(S)))
        table$setRow(rowNo = 4, values = list(parameter = "Confidence level", value = paste0(round(level_OM * 100, 1), "%")))
        table$setRow(rowNo = 5, values = list(parameter = "Design effect", value = 1))
        
        
        # --- 2. ONE PROPORTION ---
      } else if (tipo == "proporcion") {
        
        P <- self$options$P            
        e_OP <- self$options$e_OP
        level_OP <- self$options$level_OP
        
        alpha <- 1 - level_OP
        z <- qnorm(1 - alpha / 2)
        n <- ceiling((z^2 * P * (1 - P)) / (e_OP^2))
        
        table <- self$results$tableOP
        
        table$setRow(rowNo = 1, values = list(parameter = "Sample size", value = as.character(n)))
        table$setRow(rowNo = 2, values = list(parameter = "Precision", value = as.character(e_OP)))
        table$setRow(rowNo = 3, values = list(parameter = "Expected proportion", value = as.character(P)))
        table$setRow(rowNo = 4, values = list(parameter = "Confidence level", value = paste0(round(level_OP * 100, 1), "%")))
        table$setRow(rowNo = 5, values = list(parameter = "Design effect", value = 1))
        
        # --- 3. TWO MEANS DIFFERENT VARIANCE ---
      } else if (tipo == "dosMediasVD") {
        
        S1 <- self$options$S1
        S2 <- self$options$S2
        delta <- self$options$dif_IM
        power <- self$options$pow_IM
        level_IM <- self$options$level_IM
        
        zb <- qnorm(power)
        alpha <- 1 - level_IM
        z <- qnorm(1 - alpha / 2)            
        
        n1 <- ceiling(((z + zb)^2 * (S1^2 + S2^2)) / delta^2)
        n2 <- n1
        
        table <- self$results$tableIM
        
        table$setRow(rowNo = 1, values = list(parameter = "Total sample size", value = as.character(n1 + n2)))
        table$setRow(rowNo = 2, values = list(parameter = "Sample size group 1", value = as.character(n1)))
        table$setRow(rowNo = 3, values = list(parameter = "Sample size group 2", value = as.character(n2)))
        table$setRow(rowNo = 4, values = list(parameter = "Statistical power", value = as.character(power)))
        table$setRow(rowNo = 5, values = list(parameter = "Minimum detectable difference", value = as.character(delta)))
        table$setRow(rowNo = 6, values = list(parameter = "Group 1 standard deviation", value = as.character(S1)))
        table$setRow(rowNo = 7, values = list(parameter = "Group 2 standard deviation", value = as.character(S2)))
        table$setRow(rowNo = 8, values = list(parameter = "Confidence level (%)", value = paste0(round(level_IM * 100, 1), "%")))
        table$setRow(rowNo = 9, values = list(parameter = "Method", value = "Welch approximation"))
        table$setRow(rowNo = 10, values = list(parameter = "Sample size ratio", value = 1))
        
        # --- 4. TWO MEANS COMMON VARIANCE ---
      } else if (tipo == "dosMediasVC") {
        
        SC <- self$options$SC
        delta <- self$options$dif_PM
        power <- self$options$pow_PM
        level_PM <- self$options$level_PM
        
        zb <- qnorm(power)
        alpha <- 1 - level_PM
        z <- qnorm(1 - alpha / 2)            
        
        n1 <- ceiling(2 * (SC^2) * (z + zb)^2 / delta^2)
        n2 <- n1
        
        table <- self$results$tablePM
        
        table$setRow(rowNo = 1, values = list(parameter = "Estimated total sample size", value = as.character(n1 + n2)))
        table$setRow(rowNo = 2, values = list(parameter = "Sample size group 1", value = as.character(n1)))
        table$setRow(rowNo = 3, values = list(parameter = "Sample size group 2", value = as.character(n2)))
        table$setRow(rowNo = 4, values = list(parameter = "Statistical power", value = as.character(power)))
        table$setRow(rowNo = 5, values = list(parameter = "Minimum detectable difference", value = as.character(delta)))
        table$setRow(rowNo = 6, values = list(parameter = "Common standard deviation", value = as.character(SC)))
        table$setRow(rowNo = 7, values = list(parameter = "Confidence level (%)", value = paste0(round(level_PM * 100, 1), "%")))
        table$setRow(rowNo = 8, values = list(parameter = "Method", value = "Pooled"))
        table$setRow(rowNo = 9, values = list(parameter = "Sample size ratio", value = 1))
        
        # --- 5. TWO PROPORTIONS ---
      } else if (tipo == "dosProporciones") {
        
        P1 <- self$options$P1
        P2 <- self$options$P2
        delta <- self$options$dif_IP
        power <- self$options$pow_IP
        level_IP <- self$options$level_IP
        
        zb <- qnorm(power)
        alpha <- 1 - level_IP
        z <- qnorm(1 - alpha / 2)            
        
        pbar <- (P1 + P2) / 2
        
        n1 <- ceiling((z * sqrt(2 * pbar * (1 - pbar)) + zb * sqrt(P1 * (1 - P1) + P2 * (1 - P2)))^2 / (P1 - P2)^2)
        n2 <- n1
        
        table <- self$results$tableIP
        
        table$setRow(rowNo = 1, values = list(parameter = "Estimated total sample size", value = as.character(n1 + n2)))
        table$setRow(rowNo = 2, values = list(parameter = "Sample size group 1", value = as.character(n1)))
        table$setRow(rowNo = 3, values = list(parameter = "Sample size group 2", value = as.character(n2)))
        table$setRow(rowNo = 4, values = list(parameter = "Statistical power", value = as.character(power)))
        table$setRow(rowNo = 5, values = list(parameter = "Minimum detectable difference", value = as.character(delta)))
        table$setRow(rowNo = 6, values = list(parameter = "Expected proportion in group 1", value = as.character(P1)))
        table$setRow(rowNo = 7, values = list(parameter = "Expected proportion in group 2", value = as.character(P2)))
        table$setRow(rowNo = 8, values = list(parameter = "Confidence level (%)", value = paste0(round(level_IP * 100, 1), "%")))
        table$setRow(rowNo = 9, values = list(parameter = "Method", value = "Chi-square (without continuity correction)"))
        table$setRow(rowNo = 10, values = list(parameter = "Sample size ratio", value = 1))
      }
    }
  )
)