# This file is a generated template, your changes will not be overwritten

bfitClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "bfitClass",
    inherit = bfitBase,
    private = list(
      .htmlwidget = NULL,
      .boot_stat_memoised = NULL, 
      
      ###### .init function--------
      
      .init = function() {
        private$.boot_stat_memoised <- memoise::memoise(function(data, indices, stat, step) {
          d <- data[indices, ]
          res1 <- mixRasch::mixRasch(
            data = d, 
            steps = step,
            model = "RSM",
            n.c = 1
          )
          stat.raw <- res1$item.par$in.out[, stat]
          return(stat.raw)
        })
        
        if (self$options$mode == 'simple') {
          private$.htmlwidget <- HTMLWidget$new()
          
          if (is.null(self$data) | is.null(self$options$vars)) {
            self$results$instructions$setVisible(visible = TRUE)
            
          }
          
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>The traditional Rasch model is performed by <b>mixRasch</b> R package using Jonint Maximum Liklihood(JML).</li>',
                '<li>Specify <b>Step(number of category-1) and Bootstrap N</b> in the Analysis options.</li>',
                '<li>Please, be patient. The bootstrapped confidence interval is quite time-consuming.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )
          
          if (isTRUE(self$options$inplot)) {
            width <- self$options$width
            height <- self$options$height
            self$results$inplot$setSize(width, height)
          }
          
          if (isTRUE(self$options$outplot)) {
            width <- self$options$width
            height <- self$options$height
            self$results$outplot$setSize(width, height)
          }
          
        }
        
        if (self$options$mode == "complex") {
          private$.htmlwidget <- HTMLWidget$new()
          
          if (is.null(self$data) | is.null(self$options$vars1)) {
            self$results$instructions1$setVisible(visible = TRUE)
            
          }
          
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>Specify <b>Type and Bootstrap N</b> in the Analysis options.</li>',
                '<li>To use the <b>correction methods</b>, uncheck the <b>Run</b> checkbox.</li>',
                '<li>A fitted Rasch model or Partial Credit Model in R package <b>eRm</b> is used to compute bootstrap fit statistics.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )
          
          if (self$options$outfit)
            self$results$outfit$setNote("Note", "Adj.p= Adjusted p-values for Multiple Comparisons.")
          
          if (self$options$infit)
            self$results$infit$setNote("Note", "Adj.p= Adjusted p-values for Multiple Comparisons.")
          
        }
        
      },
      
      .run = function() {
        if (self$options$mode == 'complex') {
          if (is.null(self$options$vars1) |
              length(self$options$vars1) < 2)
            return()
          
          data <- self$data
          data <- na.omit(data)
          vars1 <- self$options$vars1
          bn1 <- self$options$bn1
          type <- self$options$type
          adj <- self$options$adj
          nco <- self$options$nco
          
          if (type == 'bi') {
            obj <- eRm::RM(data)
          } else if (type == 'ra') {
            obj <- eRm::PCM(data)
          }
          
          if (nco == TRUE) {
            set.seed(1234)
            fit <- iarm::boot_fit(obj, B = bn1, p.adj = 'none')
            
            #Outfit-----------------
            table <- self$results$noutfit
            
            outfit <- fit[[1]][, 1]
            outfit <- as.vector(outfit)
            
            pvalue <- fit[[1]][, 2]
            pvalue <- as.vector(pvalue)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- outfit[i]
              row[["p"]] <- pvalue[i]
              table$setRow(rowKey = vars1[i], values = row)
            }
            
            # Infit-------------
            table <- self$results$ninfit
            
            infit <- fit[[1]][, 3]
            infit <- as.vector(infit)
            
            pvalue <- fit[[1]][, 4]
            pvalue <- as.vector(pvalue)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- infit[i]
              row[["p"]] <- pvalue[i]
              table$setRow(rowKey = vars1[i], values = row)
            }
            
          } else{
            set.seed(1234)
            fit <- iarm::boot_fit(obj, B = bn1, p.adj = adj)
            
            # Outfit------------
            
            table <- self$results$outfit
            
            outfit <- fit[[1]][, 1]
            outfit <- as.vector(outfit)
            
            pvalue <- fit[[1]][, 2]
            pvalue <- as.vector(pvalue)
            
            padj <- fit[[1]][, 3]
            padj <- as.vector(padj)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- outfit[i]
              row[["p"]] <- pvalue[i]
              row[["adp"]] <- padj[i]
              table$setRow(rowKey = vars1[i], values = row)
            }
            # Infit table-------------
            table <- self$results$infit
            
            infit <- fit[[1]][, 4]
            infit <- as.vector(infit)
            
            pvalue <- fit[[1]][, 5]
            pvalue <- as.vector(pvalue)
            
            padj <- fit[[1]][, 6]
            padj <- as.vector(padj)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- infit[i]
              row[["p"]] <- pvalue[i]
              row[["adp"]] <- padj[i]
              table$setRow(rowKey = vars1[i], values = row)
            }
          }
        }
        data <- self$data
        vars <- self$options$vars
        
        # Ready--------
        
        ready <- TRUE
        
        if (is.null(self$options$vars) |
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          data <- private$.cleanData()
          results <- private$.compute(data)
          
          # populate Boot Fit table-------------
          private$.populateInTable(results)
          private$.populateOutTable(results)
          
          # plot------
          private$.prepareInPlot(results)
          private$.prepareOutPlot(results)
        }
      },
      
      .compute = function(data) {
        vars <- self$options$vars
        step <- self$options$step
        bn <- self$options$bn

        self$results$text$setContent("Starting bootstrap. Please wait...")
        #self$setStatus('Starting bootstrap. . .')
        set.seed(1234)
        if(bn > 1000) {
          chunk_size <- 500
          chunks <- ceiling(bn / chunk_size)
          
          infit_results <- list()
          outfit_results <- list()
          
          for(i in 1:chunks) {
            start_idx <- (i-1) * chunk_size + 1
            end_idx <- min(i * chunk_size, bn)
            current_size <- end_idx - start_idx + 1
            
            # self$setStatus(paste0('Computing... (', round(100 * (i-1) / chunks), '%)'))
            # Sys.sleep(0.1)
            #self$results$text$setContent(sprintf("진행 중: %d%% 완료", round(100 * (i-1) / chunks)))
            boot.in.chunk <- boot::boot(
              data = data,
              statistic = function(d, idx) {
                return(private$.boot_stat_memoised(d, idx, 1, step))
              },
              R = current_size
            )
            boot.out.chunk <- boot::boot(
              data = data,
              statistic = function(d, idx) {
                return(private$.boot_stat_memoised(d, idx, 3, step))
              },
              R = current_size
            )
            
            infit_results[[i]] <- boot.in.chunk$t
            outfit_results[[i]] <- boot.out.chunk$t
            
            if(i == 1) {
              infit.raw <- boot.in.chunk$t0
              outfit.raw <- boot.out.chunk$t0
            }
          }
          #self$setStatus('Completed!')
          infit <- do.call(rbind, infit_results)
          outfit <- do.call(rbind, outfit_results)
          
          infit <- infit[1:bn,]
          outfit <- outfit[1:bn,]
        } else {

          set.seed(1234)
          
          boot.in <- boot::boot(
            data = data,
            statistic = function(d, idx) {
              return(private$.boot_stat_memoised(d, idx, 1, step))
            },
            R = bn
          )
          
          boot.out <- boot::boot(
            data = data,
            statistic = function(d, idx) {
              return(private$.boot_stat_memoised(d, idx, 3, step))
            },
            R = bn
          )
          
          infit.raw <- boot.in$t0
          infit <- boot.in$t
          outfit.raw <- boot.out$t0
          outfit <- boot.out$t
        }
        
        self$results$text1$setContent("Bootstrapping completed.")
        
        
        infitlow <- apply(infit, 2, quantile, prob = 0.025)
        infithigh <- apply(infit, 2, quantile, prob = 0.975)
        outfitlow <- apply(outfit, 2, quantile, prob = 0.025)
        outfithigh <- apply(outfit, 2, quantile, prob = 0.975)
        
        results <-
          list(
            'infit' = infit.raw,
            'outfit' = outfit.raw,
            'infitlow' = infitlow,
            'infithigh' = infithigh,
            'outfitlow' = outfitlow,
            'outfithigh' = outfithigh
          )
      },
      
      # Populate boot table------------
      .populateInTable = function(results) {
        table <- self$results$item$binfit
        vars <- self$options$vars
        
        # results------
        infit <- results$infit
        infitlow <- results$infitlow
        infithigh <- results$infithigh
        
        rows_data <- list()
        for (i in seq_along(vars)) {
          row <- list()
          row[["infit"]] <- infit[i]
          row[["infitlow"]] <- infitlow[i]
          row[["infithigh"]] <- infithigh[i]
          table$setRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateOutTable = function(results) {
        table <- self$results$item$boutfit
        vars <- self$options$vars
        outfit <- results$outfit
        outfitlow <- results$outfitlow
        outfithigh <- results$outfithigh
        
        rows_data <- list()
        for (i in seq_along(vars)) {
          row <- list()
          row[["outfit"]] <- outfit[i]
          row[['outfitlow']] <- outfitlow[i]
          row[['outfithigh']] <- outfithigh[i]
          table$setRow(rowKey = vars[i], values = row)
        }
      },
      
      # prepare confidence interval plot-----------
      
      .prepareInPlot = function(results) {
        item <- self$options$vars
        
        infit <- results$infit
        infitlow <- results$infitlow
        infithigh <- results$infithigh
        
        infit1 <- data.frame(
          item = factor(item, levels = item),  
          infit = infit,
          infitlow = infitlow,
          infithigh = infithigh,
          stringsAsFactors = FALSE
        )
        
        image <- self$results$inplot
        image$setState(infit1)
      },
      
      .inPlot = function(image, ggtheme, theme, ...) {
        inplot <- self$options$inplot
        
        if (is.null(self$options$vars))
          return()
        
        infit1 <- image$state
        
        plot <- ggplot(infit1, aes(x = item, y = infit, color = item)) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = infitlow, ymax = infithigh), width = 0.3) +
          geom_hline(yintercept = 1,
                     linetype = "dashed",
                     color = "red") +
          labs(title = "", x = "Item", y = "Infit") +
          scale_color_brewer(palette = "Set1") +
          theme_minimal(base_size = 15) +
          guides(color = "none") # 범례 제거
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot)
        TRUE
      },
      
      .prepareOutPlot = function(results) {
        item <- self$options$vars
        
        outfit <- results$outfit
        outfitlow <- results$outfitlow
        outfithigh <- results$outfithigh
        
        outfit1 <- data.frame(
          item = factor(item, levels = item),  
          outfit = outfit,
          outfitlow = outfitlow,
          outfithigh = outfithigh,
          stringsAsFactors = FALSE
        )
        
        image <- self$results$outplot
        image$setState(outfit1)
      },
      
      .outPlot = function(image, ggtheme, theme, ...) {
        outplot <- self$options$outplot
        
        if (is.null(self$options$vars))
          return()
        
        outfit1 <- image$state
        
        plot <- ggplot(outfit1, aes(x = item, y = outfit, color = item)) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = outfitlow, ymax = outfithigh), width = 0.3) +
          geom_hline(yintercept = 1,
                     linetype = "dashed",
                     color = "red") +
          labs(title = "", x = "Item", y = "Outfit") +
          scale_color_brewer(palette = "Set1") +
          theme_minimal(base_size = 15) +
          guides(color = "none") # 범례 제거
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot)
        TRUE
      },
      
      #### Helper functions =================================
      
      .cleanData = function() {
        items <- self$options$vars
        selected_data <- self$data[items]
        for (item in items) {
          selected_data[[item]] <- jmvcore::toNumeric(selected_data[[item]])
        }
        selected_data <- na.omit(selected_data)
        return(selected_data)
      }
    )
  )