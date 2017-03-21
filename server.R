library(shiny)
library(shinyjs)
library(datasets)
library(grid)
#library(metafor)
library(colourpicker)
library(sas7bdat)
library(dplyr)
library(tools)
library(Jasper)
library(DT)
library(XML)
#library(data.table)
library(rhandsontable)
library(listviewer)
Jasper_loaded <- FALSE

#' user defined functions in another file
debugSource("server_func.R")
#' session
shinyServer(function(input, output, session) {
  #' session variables
  #' Create the reactiveValues holding the data
  myplot  <- reactiveValues() #live data
  #' Create A NULL object list, and will be filled later
  myplot$opts <-
    list()  #session variable,  default plot options definations
  myplot$defs <-
    list()  #session variable,  page and plot graph definations
  myplot$dataset <-
    list()  #session variable,  data and row graph definations
  PageSize <- vector() #session variable, page size  in ppi
  myplot_live <- reactiveValues() #live data
  coldefs <- reactiveValues() #columns definations
  textcols <- reactiveValues() #text columns
  #auto-update page size
  observe({
    input$orient
    input$page.size
    isolate({
      if (input$page.size != "custom")
        
      {
        PageSize <<-
          getPageSize(page_type = input$page.size,
                      page_orientation = input$orient)
        updateNumericInput(session, "width", value = PageSize[1])
        updateNumericInput(session, "height", value = PageSize[2])
      }
      
    })
  })
  # sync with the changes in tbl_coldefs
  #' calculate the selected page dateset from  data and graphdef files
  getForestData <- reactive({
    #skip it if no data/defs loaded
    if (length(myplot$defs) == 0 | length(myplot$dataset) == 0)
    {
      return()
    }
    if (input$npage == "")
      return()
    page_selected <- input$npage
    current_page <- myplot$defs[[page_selected]]
    # get forest list
    forest_list <-
      names(current_page)[grepl("forest", names(current_page))]
    
    
    # forest number 0,1, more 1
    if (is.null(forest_list))
    {
      cat ("no forest plot defined in this selected page:",
           page_selected)
      reurn()
    } else if (length(forest_list) > 1)
    {
      multiple.forest <- TRUE
    } else
    {
      multiple.forest <- FALSE
    }
    
    
    # restricting data to the current page
    cpage <- list()
    forest_name_list <- vector()
    for (nforest in  forest_list)
    {
      forest_name_list <-
        c(forest_name_list, myplot$defs[[page_selected]][[nforest]]$name)
    }
    cpage$rowdefs <-
      subset(myplot$dataset$rowdefs, figref %in% forest_name_list)
    cpage$rawdata <-
      subset(myplot$dataset$rawdata,
             analref %in% cpage$rowdefs$analref)
    # looping  by forest
    index = 0
    cols.forest.multiple <- vector()
    for (nforest in forest_list)
    {
      index = index + 1
      current_forest <- current_page[[nforest]]
      cols.text <-
        intersect(current_forest$columns, names(cpage$rawdata))
      cols.text.decimalplaces <-
        current_forest$columns.decimalplaces
      cols.text.type <-
        current_forest$columns.type
      cols.forest <-
        c(current_forest$estimatecol,
          current_forest$lcicol,
          current_forest$ucicol)
      #multiple forest
      cols.forest.multiple <-
        rbind(cols.forest.multiple,
              lapply(cols.forest, paste0, as.character(index)))
      #adding index 0,1,2...
      if (multiple.forest)
      {
        cols.forest <- lapply(cols.forest, paste0, as.character(index))
      }
      #id_list <- subset(cpage$rowdefs, figref == nforest)[ID, ]
      # parsing and  adding data raw by raw
      for (iorder in min(cpage$rowdefs[, "order"]):max(cpage$rowdefs[, "order"]))
      {
        current_row <- list()
        current_row$analref <- data.frame()
        current_rowdefs <- subset(cpage$rowdefs, order == iorder)
        
        if (nrow(current_rowdefs) == 1)
        {
          aref <- current_rowdefs$analref
          current_row$analref <- as.character(aref)
          names(current_row$analref) <- "analref"
          current_row$jasper <- subset(cpage$rowdefs,
                                       order == iorder,
                                       select = names(cpage$rowdefs)[grepl("jasper", names(cpage$rowdefs))])
          current_row$alt <- subset(cpage$rowdefs,
                                    order == iorder,
                                    select = names(cpage$rowdefs)[grepl("^alt", names(cpage$rowdefs))])
          if (current_rowdefs[["jasper.type"]] == "blank")
          {
            # replacing the description column with the text in the alt.description 
            current_row$text <-
              data.frame(matrix(NA, ncol = length(cols.text), nrow = 1))
            names(current_row$text) <- cols.text
            current_row$text [1] <-  current_row$alt$alt.description
            current_row$forest <-
              data.frame(matrix(
                NA,
                ncol = length(cols.forest),
                nrow = 1
              ))
            #vector(mode = "numeric", length = length(cols.forest))
            names(current_row$forest) <- cols.forest
          }
          
          if (current_rowdefs$type == "subgroup")
          {
            if (current_rowdefs[["jasper.type"]] == "blank")
            {
              #  getting  subgroup heading from  subgrnvarlabel if appliable
              if (subset(cpage$rawdata, analref == as.character(aref))$subgrvarlabel[1] !=
                  "")
                current_row$text [1] <-
                  as.character(subset(cpage$rawdata, analref == as.character(aref))$subgrvarlabel[1])
              else
              {
                # otherwise use the description as subgroup heading
                
                current_row$text [1] <-
                  as.character(subset(cpage$rawdata, analref == as.character(aref))$description[1])
                
              }
            }
            else if (current_rowdefs[["jasper.type"]] == "data")
            {
              #get the data from subgr column
              subcols.text <- cols.text
              subcols.text[[1]] <- "subgr"
              current_row$text <-
                subset(cpage$rawdata,
                       analref == as.character(aref),
                       select = subcols.text)
              names(current_row$text) <-
                cols.text # restore the column names
              current_row$forest <-
                subset(cpage$rawdata,
                       analref == as.character(aref),
                       select = cols.forest)
              #repeat jasper rows
              jasper_row <- current_row$jasper
              alt_row <- current_row$alt
              for (irow in 2:nrow(current_row$text))
              {
                current_row$jasper <- rbind(current_row$jasper, jasper_row)
                current_row$alt <- rbind(current_row$alt, alt_row)
              }
            } else
            {
              cat("undefined jasper type:", current_rowdefs[["jasper.type"]])
              #print("subgroup")
            }
            
          }
          else if (current_rowdefs$type == "event" ||
                   current_rowdefs$type == "mainevent")
            #non subgroup data
          {
            current_row$text <-
              subset(cpage$rawdata,
                     analref == as.character(aref),
                     select = cols.text)
            
            current_row$forest <-
              subset(cpage$rawdata,
                     analref == as.character(aref),
                     select = cols.forest)
            
            #cols.text <- current_forest$columns
            
            
          } else if (current_rowdefs$type == "blank")
          {
            print("blank line")
          } else
          {
            cat ("data type not found:", current_rowdefs$type)
          }
          # percenting or rounding up
          for (i in 1:length(current_row$text))
          {
            if (grepl("pct", names(current_row$text)[i]))
            {
              current_row$text [i] <- current_row$text [i] * 100
            }
            if (!is.na(current_row$text [[i]]))
            {
              if (is.numeric(current_row$text [[i]]))
                current_row$text [i] <-
                  round(current_row$text [i], cols.text.decimalplaces[i])
            }
          }
          if (is.null(cpage$data$forest[[nforest]]))
          {
            cpage$data$forest[[nforest]] <-
              cbind(
                current_row$analref,
                current_row$text,
                current_row$alt,
                current_row$forest,
                current_row$jasper
              )
          }
          else
          {
            cpage$data$forest[[nforest]] <-
              rbind(
                cpage$data$forest[[nforest]] ,
                cbind(
                  current_row$analref,
                  current_row$text,
                  current_row$alt,
                  current_row$forest,
                  current_row$jasper
                )
              )
          }
          
          
        } else
        {
          print("missing or depulicated order number in rowdefs, ignored")
        }
        
      }
      # dealing with alt fields
      # replacing the desctiption with alt desctiption if appliable
      cpage$data$forest[[nforest]]$description <-
        ifelse(
          cpage$data$forest[[nforest]]$alt.description == "",
          as.character(cpage$data$forest[[nforest]]$description),
          as.character(cpage$data$forest[[nforest]]$alt.description)
        )
      #
      #		Merck formatting standards:
      #   If the value is less than 0.001 display "<0.001"
      #   If the value is greater than 0.999 display ">0.999"
      if (input$format_standards == "merck")
      {
        if (input$columns.pvalues %in% names(cpage$data$forest[[nforest]]))
        {
          cpage$data$forest[[nforest]]$alt.pvalue.print <-
            ifelse(
              cpage$data$forest[[nforest]][input$columns.pvalues] < 0.001,
              "<0.001",
              as.character(cpage$data$forest[[nforest]]$alt.description)
            )
          cpage$data$forest[[nforest]]$alt.pvalue.print <-
            ifelse(
              cpage$data$forest[[nforest]][input$columns.pvalues] > 0.999,
              ">0.999",
              as.character(cpage$data$forest[[nforest]]$alt.description)
            )
        }
      }
      
      #	Show 2 columns of P value:
      # First column without taking account of multiplicity (i.e. show all p values)
      #   Second column taking account of multiplicity:
      #     show p value for DAP 1
      #    if p<0.05 for DAP1, show p value for DAP2.4.1, 2.4.2 and 2.4.3
      #      (else show p value for 2.4.1, 2.4.2 and 2.4.3 as “-“)
      
      #check if any such condition exists
      if (any(!(is.na(
        cpage$data$forest[[nforest]]$alt.pvalue.clause
      )) & !(cpage$data$forest[[nforest]]$alt.pvalue.clause == "")))
      {
        if (!("Pcorrected" %in%  myplot$dataset$colsdefs[[page_selected]][[nforest]]$columns))
        {
          # adding new  coldefs
          myplot$dataset$colsdefs[[page_selected]][[nforest]] <-
            rbind(
              myplot$dataset$colsdefs[[page_selected]][[nforest]],
              c("Pcorrected", "Pcorrected", 98, "centre", "num", 3)
            )
          #update the reactive variables
          coldefs$dat <-
            myplot$dataset$colsdefs[[input$npage]][[input$nforest]]
        }
        if (!("Pcorrected" %in% names(cpage$data$forest[[nforest]])))
        {
          # copy P values to this column and will be modified later
          cpage$data$forest[[nforest]][["Pcorrected"]] <-
            cpage$data$forest[[nforest]][[input$columns.pvalues]]
        }
        
        
        # cpage$data$forest[[nforest]][["Pcorrected"]]<- ifelse(cpage$data$forest[[nforest]][input$columns.pvalues]<0.001,
        # cpage$data$forest[[nforest]]$alt.pvalue.print,
        #	 cpage$data$forest[[nforest]][["Pcorrected"]]
        
      }
      
      
      
      myplot$dataset$data[[page_selected]] <<-
        cpage$data$forest[nforest]
    }
    
    
    
  })
  SaveForestData <- reactive({
    if (is.null(myplot$dataset$data[[input$npage]]))
    {
      getForestData()
    }
    mydata <- myplot$dataset$data[[input$npage]][[input$nforest]]
    if (!is.null(mydata))
    {
      fileName <- input$data
      if (file.exists(fileName))
      {
        file.rename(fileName, paste0(fileName, ".bak"))
      }
      write.csv(mydata,
                file = input$data,
                row.names = FALSE)
    }
    else
    {
      print("no data saved")
    }
  })
  observe({
    if (!is.null(input$tbl_coldefs))
    {
      DF <- hot_to_r(input$tbl_coldefs)
      if (!isTRUE(all.equal(coldefs$dat, DF)))
      {
        coldefs$dat <- hot_to_r(input$tbl_coldefs)
        print("coldefs have been edited")
      }
      if (!isTRUE(all.equal(coldefs$dat$columns, DF$columns)))
      {
        #recalculate dataset and get new data from rawdata if needed
        getForestData()
        print("columns have been changed, recalculated the dataset")
      }
      coldefs$dat <- DF
      print("The changes on the column definations have been synced")
    }
  })
  # sync with the changes in tbl_text
  observe({
    if (!is.null(input$tbl_text))
    {
      DF <- hot_to_r(input$tbl_text)
      if (!isTRUE(all.equal(textcols$dat, DF)))
      {
        textcols$dat <- hot_to_r(input$tbl_text)
        print("The changes on the text table have been synced")
      }
    }
    
  })
  # sync with  the changes from input variables
  observeEvent(input$save, {
    #sync the input values with myplot$defs
    isolate({
      no_of_changes <- 0
      #update page-related fields
      for (ninput in intersect(names(myplot$opts$page), names(input)))
      {
        # unlist text string like "1,2,3" into "1 2 3" in some inputs if needed
        # e.g. numeric array
        if (myplot$opts[["page"]][[ninput]]$shiny == "TextInput" &&
            myplot$opts[["page"]][[ninput]]$expected_type == "num" &&
            grepl(",", input[[ninput]]))
        {
          myinput <- as.numeric(unlist(strsplit(input[[ninput]], ",")))
        } else if (myplot$opts[["page"]][[ninput]]$shiny == "ColourInput")
        {
          myinput <- as.character(input[[ninput]])
        }
        else
          #unchanged
        {
          myinput <- input[[ninput]]
        }
        if (!isTRUE(all.equal(myplot$defs[[input$npage]][[ninput]], myinput)))
        {
          cat(paste0(ninput, " is changed\n"))
          cat(paste0(myplot$defs[[input$npage]][[ninput]], " is changed to ", myinput, "\n"))
          myplot$defs[[input$npage]][[ninput]] <<- myinput
          no_of_changes <- no_of_changes + 1
        }
      }
      #update forest-related fields
      forest_options <- myplot$opts[["page"]][["forest"]]
      for (ninput in intersect(names(myplot$opts$page$forest), names(input)))
      {
        if (forest_options[[ninput]]$shiny == "TextInput" &&
            forest_options[[ninput]]$expected_type == "num" &&
            grepl(",", input[[ninput]]))
        {
          myinput <- as.numeric(unlist(strsplit(input[[ninput]], ",")))
        } else
        {
          myinput <- input[[ninput]]
        }
        
        if (!isTRUE(all.equal(myplot$defs[[input$npage]][[input$nforest]][[ninput]], myinput)))
        {
          cat(paste0(ninput, " is changed\n"))
          cat(paste0(myplot$defs[[input$npage]][[input$nforest]][[ninput]], " is changed to ", myinput, "\n"))
          myplot$defs[[input$npage]][[input$nforest]][[ninput]] <<-
            myinput #updating
          no_of_changes <- no_of_changes + 1
        }
        
      }
      # update columns definations
      if (!is.null(coldefs$dat))
      {
        myplot$defs[[input$npage]][[input$nforest]][names(coldefs$dat)] <<-
          coldefs$dat
        print(coldefs$dat)
      }
      # update  text columns
      if (!is.null(textcols$dat))
      {
        df <- textcols$dat
        row.names(df) <- textcols$dat$name
        df <- df[, !(colnames(df) %in% "name")]
        text_name_list <-
          names(myplot$defs[[input$npage]])[grepl("^text", names(myplot$defs[[input$npage]]))]
        
        for (ntext in  row.names(df))
        {
          if (!(ntext %in% text_name_list)) {
            myplot$defs[[input$npage]][[ntext]] <<- df[ntext, ]
            cat(paste0(ntext, " is added\n"))
          }
          else if (!isTRUE(all.equal(
            data.frame(myplot$defs[[input$npage]][[ntext]],
                       stringsAsFactors = FALSE),
            df[ntext, ],
            check.attributes = FALSE
          )))
          {
            myplot$defs[[input$npage]][[ntext]] <<- df[ntext, ]
            cat(paste0(ntext, " is updated\n"))
            
          } else
          {
            cat(paste0(ntext, " is unchanged\n"))
            
          }
        }
        for (ntext in text_name_list)
        {
          if (!(ntext %in%  row.names(df))) {
            myplot$defs[[input$npage]][[ntext]] <<- NULL
            cat(paste0(ntext, " is deleteded\n"))
          }
        }
      }
      
      print(textcols$dat)
      
    })
  })
  #save myplot$defs into a file
  observeEvent(input$saveGraphdef, {
    isolate({
      if (length(myplot$defs) == 0)
      {
        print("no graph defs defined")
      } else
      {
        fileName <- input$graphdef_file$name
        if (file.exists(fileName))
        {
          file.copy(fileName, paste0(fileName, ".bak"), overwrite = TRUE)
        }
        trials.saveplotdefxml(myplot$defs, filename = fileName)
        cat(paste0("Graph defs file ", fileName, " has been updated/saved\n"))
      }
      #trials.saveplotdefxml(myplot$defs, file = NULL)
    })
  })
  observeEvent(input$savePlot, {
    #sync the input  with myplot$defs
    #isolate({
      if (length(myplot$defs) == 0)
      {
        print("no graph defs defined")
      } else
      {
        # get pages list
        pages <- names(myplot$defs)[grepl("page", names(myplot$defs))]
        print(pages)
        # page 1
        if (is.null(pages))
          return()
        else{
          for (npage in pages)
          {
            updateSelectInput(session,
                              "npage",
                              choices = pages,
                              selected = npage)
            print(input$npage)
            print(input$nforest)
            SaveForestData()
          }
        }
        
        trials(
          plot.def = input$graphdef_file$name,
          plot.def.mode = "XML",
          output_folder = input$plotoutput
        )
        
        cat(paste(
          "Graph(s) has been generated from : ",
          input$graphdef_file$name,
          "\n"
        ))
        
      }
    #})
  })
  
  #autoload a forest page when the input changes
  observeEvent(c(input$npage, input$nforest), {
    if (input$npage == "")
      return()
    current_page <- myplot$defs[[input$npage]]
    if (input$nforest == "")
      return()
    current_forest <- current_page[[input$nforest]]
    forest_options <- myplot$opts[["page"]][["forest"]]
    # updating inputs
    # get columns  defs
    forest_columns_list <- vector()
    #forest_column_list <-
    #  names(current_forest)[grepl("^column", names(current_forest))]
    # excluding heading style ?
    for (selected_col in  (names(current_forest)[grepl("^column", names(current_forest))]))
    {
      if (is.null(forest_options[[selected_col]]$shiny))
      {
        cat(paste0(forest_options[[selected_col]], " is expected a shiny value!\n"))
        returnValue("missing options shiny value")
      }
      if (forest_options[[selected_col]]$shiny == "rhandsontable")
      {
        forest_columns_list <-
          c(forest_columns_list, current_forest[selected_col])
        #cat(paste0(
        #  current_forest[selected_col],
        #  " is added into the columns rhandsontable!\n"
        #))
      }
    }
    
    coldefs$dat <-
      data.frame(forest_columns_list, stringsAsFactors = FALSE)
    myplot$dataset$colsdefs[[input$npage]][[input$nforest]] <<-
      data.frame(forest_columns_list, stringsAsFactors = FALSE)
    
    # must-have fields
    for (ninput in names(input))
    {
      #print(ninput)
      
      if (!is.null(current_forest[[ninput]]))
      {
        print(current_forest[ninput])
        if (is.null(forest_options[[ninput]]$shiny))
        {
          returnValue("missing value")
        }
        if (forest_options[[ninput]]$shiny == "TextInput")
          updateTextInput(session, ninput, value = current_forest[[ninput]])
        else  if (forest_options[[ninput]]$shiny == "RadioButtons")
          updateRadioButtons(session, ninput, selected = current_forest[[ninput]])
        else  if (forest_options[[ninput]]$shiny == "SliderInput")
          updateSliderInput(session, ninput, value = current_forest[[ninput]])
        else if (forest_options[[ninput]]$shiny == "ColourInput")
          updateColourInput(session, ninput, value = current_forest[[ninput]])
        else if (forest_options[[ninput]]$shiny == "CheckboxInput")
          updateCheckboxInput(session, ninput, value = toupper(current_forest[[ninput]]))
        else
        {
          cat(paste0(ninput, " is an undefined shiny input!\n"))
        }
      }
      
      # updateRadioButtons(session, ninput, selected = current_forest[[ninput]])
      # if (grepl("column", ninput))
      # {
      #   updateSelectizeInput(session,
      #                        ninput,
      #                        choices = current_forest[[ninput]],
      #                        selected = current_forest[[ninput]])
      # }
      # updateTextInput(session, ninput, value = current_forest[[ninput]])
      # if (is.numeric(current_forest[[ninput]]))
      #   updateSliderInput(session, ninput, value = current_forest[[ninput]])
      # #updateCheckboxInput(session, ninput, value = current_forest[[ninput]])
      # #updateCheckboxInput(session, ninput, value = toupper(current_forest[[ninput]]))
    }
    
    
  })
  observeEvent(input$npage, {
    if (input$npage == "")
      return()
    # update the forest selector to NULL
    updateSelectInput(session,
                      "nforest",
                      choices = NULL,
                      selected = NULL)
    
    current_page <- myplot$defs[[input$npage]]
    # updating inputs
    # must-have fields
    for (ninput in names(input))
    {
      print(ninput)
      
      if (!is.null(current_page[[ninput]]))
      {
        print(current_page[ninput])
        if (myplot$opts[["page"]][[ninput]]$shiny == "TextInput")
          updateTextInput(session, ninput, value = current_page[[ninput]])
        else  if (myplot$opts[["page"]][[ninput]]$shiny == "RadioButtons")
          updateRadioButtons(session, ninput, selected = current_page[[ninput]])
        else  if (myplot$opts[["page"]][[ninput]]$shiny == "SliderInput")
          updateSliderInput(session, ninput, value = current_page[[ninput]])
        else if (myplot$opts[["page"]][[ninput]]$shiny == "ColourInput")
          updateColourInput(session, ninput, value = current_page[[ninput]])
        else if (myplot$opts[["page"]][[ninput]]$shiny == "CheckboxInput")
          updateCheckboxInput(session, ninput, value = toupper(current_page[[ninput]]))
        else
        {
          print("undefined shiny input!")
        }
      }
    }
    # get forest list
    forest_list <-
      names(current_page)[grepl("forest", names(current_page))]
    # forest 1
    if (!is.null(forest_list))
    {
      current_forest_no <- forest_list[1]
      # update the forest selector
      updateSelectInput(session,
                        "nforest",
                        choices = forest_list,
                        selected = current_forest_no)
    }
    # get text list
    text_list <-
      names(current_page)[grepl("^text", names(current_page))]
    df <- data.frame()
    for (ntext in text_list)
    {
      df <-
        rbind(df, data.frame(myplot$defs[[input$npage]][[ntext]], stringsAsFactors =
                               FALSE))
    }
    text_list_df <- data.frame(text_list)
    names(text_list_df) <- "name"
    textcols$dat <<- cbind(data.frame(text_list_df), df)
    
  })
  obs <- observeEvent(input$graphdef_file, {
    #validate(need(is.null(input$graphdef_file), "Please select a ghaphdef file"))
    fileName <- input$graphdef_file$name
    myplot$defs  <<- trials.readplotdefxml(fileName)
    # validate the options
    myplot$defs <<- trials.validateplotdef(myplot$defs)
    # get pages list
    pages <- names(myplot$defs)[grepl("page", names(myplot$defs))]
    # page 1
    if (is.null(pages))
      return()
    current_page_no <- pages[1]
    # update the page selector
    updateSelectInput(session, "npage", choices = pages, selected = current_page_no)
    
    
  })
  obs <- observeEvent(input$style_file, {
    #validate(need(is.null(input$graphdef_file), "Please select a ghaphdef file"))
    fileName <- input$style_file$name
    options.xml <- xmlInternalTreeParse(fileName)
    myplot$opts <<- xmlToList(options.xml)
    
    #myplot$opts  <<- trials.readplotdefxml(fileName)
    # validate the options
    #myplot$opts <<- trials.validateplotdef(myplot$opts)
  })
  observeEvent(input$columns, {
    print(input$columns)
    
  })
  # When the client ends the session, suspend the observer.
  # Otherwise, the observer could keep running after the client
  # ends the session.
  session$onSessionEnded(function() {
    #obs$suspend()
    
    # Also clean up
    unlink(myplot) #deleting
  })
  #' Return the requested plot type
  getPlotType <- reactive({
    ret <- switch(
      input$type,
      forest = "Forest plot",
      #
      survival = "survival curve",
      #
      linear = "linear"
    )
    
  })
  #' Return the requested output type
  getOutputType <- reactive({
    ret <- switch(input$output.type,
                  gui = "GUI",
                  # on screen
                  svg = "SVG",
                  # SVG
                  pdf = "PDF")
  })
  #' Return the requested page type
  getPageType <- reactive({
    switch(input$page.size,
           a4 = "A4",
           # A4 lanscape
           letter = "USA Letter",
           # A4 Portrait
           ppt = "PPT")
  })
  #' Return the requested page orient
  getPageOrientation <- reactive({
    switch(input$orient,
           landscape = "LANDSCAPE",
           # A4 lanscape
           portrait = "PORTRAIT") # A4 Portrait
  })
  #' Return the requested dateset from a data file
  observeEvent(input$data_file, {
    validate(need(
      is.null(input$sytle_file),
      "Please select a style file first"
    ))
    file1 <- input$data_file
    if (is.null(file1)) {
      return(NULL)
    } else
    {
      # reading from a csv data file
      if (identical(tolower(tools::file_ext(file1$name)), "csv")) {
        myplot$dataset$rawdata <<- read.csv(file1$datapath)
      } else if (identical(tolower(tools::file_ext(file1$name)), "sas7bdat"))
      {
        # reading from a SAS data set
        if (identical(tolower(tools::file_ext(file1$name)), "sas7bdat")) {
          myplot$dataset$rawdata <<- read.sas7bdat(file1$name) #SAS file
        }
      }  else {
        stop("no other mode supported")
      }
      # read row definations from the file
      file_rowdefs <-
        paste0(file_path_sans_ext(file1), ".rowdefs")
      if (!file.exists(file_rowdefs[1])) {
        stop("no rowdef file")
      } #reporting error
      else
      {
        myplot$dataset$rowdefs <<- read.csv(file = file_rowdefs[1])
      }
      #lowercase		 
      names(myplot$dataset$rawdata )<-tolower(names(myplot$dataset$rawdata ))	
    }
    
  })
  
  
  
  SavePlotDefs <- reactive({
    if (is.null(myplot$defs[[input$npage]]))
    {
      print("no defs saved")
    } else
    {
      fileName <- input$graphdef_file$name
      if (file.exists(fileName))
      {
        file.rename(fileName, paste0(fileName, ".bak"))
      }
      trials.saveplotdefxml(myplot$defs, filename = fileName)
    }
  })
  # Return the requested dateset
  getGraphdef <- reactive({
    validate(need(
      is.null(input$graphdef_file),
      "Please select a ghaphdef file"
    ))
    fileName <- input$graphdef_file
    myplot$defs  <<- trials.readplotdefxml(fileName)
    # validate the options
    myplot$defs <<- trials.validateplotdef(myplot$defs)
    
  })
  #get style
  getStyle <- reactive({
    file2 <- input$style_file
    if (is.null(file2)) {
      return(NULL)
    }
    fileName <- input$input$style_file$name
    myplot$opts <<- trials.readplotdefxml(fileName)
    # validate the options
    myplot$opts <<- trials.validateplotdef(myplot$defs)
    #read.csv(file2$datapath)
  })
  #get Figure List
  getFigureList <- reactive({
    file_figure_list <- input$figure_list
    if (is.null(file_figure_list)) {
      print("no figure list found")
      return(NULL)
    }
    figure_list <-
      read.csv(file_figure_list$datapath)
    
    updateSelectInput(
      session,
      "figure_selector",
      length(figure_list$figure_name),
      choices = igure_list$figure_name,
      selected =  tail(gure_list$figure_name, 1)
    )
    
  })
  # Return the requested dateset
  getGraphdef <- reactive({
    validate(need(
      is.null(input$graphdef_file),
      "Please select a ghaphdef file"
    ))
    fileName <- input$graphdef_file
    myplot$defs  <<- trials.readplotdefxml(fileName)
    # validate the options
    myplot$defs <<- trials.validateplotdef(myplot$defs)
    
  })
  
  getFigureList <- reactive({
    file_figure_list <- input$figure_list
    
    if (is.null(file_figure_list)) {
      print("no figure list found")
      return(NULL)
    }
    figure_list <-
      read.csv(file_figure_list$datapath)
    
    updateSelectInput(
      session,
      "figure_selector",
      length(figure_list$figure_name),
      choices = igure_list$figure_name,
      selected =  tail(gure_list$figure_name, 1)
    )
    
  })
  output$tbl_coldefs <- renderRHandsontable({
    if (!is.null(coldefs$dat))
    {
      rhandsontable(data.frame(coldefs$dat),
                    useTypes = TRUE,
                    stretchH = "all")  %>%
        hot_col("columns.headings", allowInvalid = TRUE)
    }
  })
  output$tbl_text <- renderRHandsontable({
    if (!is.null(textcols$dat))
      rhandsontable(data.frame(textcols$dat),
                    useTypes = TRUE,
                    stretchH = "all") %>%
      hot_col("name", allowInvalid = TRUE)
  })
  output$fileUploaded <- reactive({
    #return(!is.null(getForestData()))
  })
  #outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # Generate a summary of the dataset
  output$settings <- renderPrint({
    PageSize <-
      getPageSize(page_type = input$page.size,
                  page_orientation = input$orient)
    updateNumericInput(session, "width", value = PageSize[1])
    updateNumericInput(session, "height", value = PageSize[2])
    cat(cat(paste("       Plot Type:", getPlotType(), "\n")),
        cat(paste("       Page Size:", getPageType(), "\n")),
        cat(paste(
          "Page Orientation:",
          getPageOrientation(),
          "\n"
        )),
        cat(paste("     Output Type:", getOutputType(), "\n")),
        cat(paste(
          "       Page Size:",
          toString(PageSize[1]),
          "x",
          toString(PageSize[2]) ,
          "\n"
        )))
    
  })
  
  output$jsonedit1 <-
    renderJsonedit({
      if (is.null(myplot_live$dat))
      {
        myplot_live$dat <<- myplot
      }
      
      jsonedit(
        reactiveValuesToList(myplot_live$dat) ,
        onChange = htmlwidgets::JS(
          "
          function(){
          Shiny.onInputChange(
          'jsonedit1_change',
          HTMLWidgets.getInstance(document.getElementById('jsonedit1')).editor.getText()
          );
          }
          "
        )
        )
})
  observeEvent(input$jsonedit1_change, {
    myplot_live$dat <<- as.list(input$jsonedit1_change)
  })
  # Show the first "n" observations
  output$view <- DT::renderDataTable({
    if (is.null(input$data_file)) {
      return(NULL)
    }
    rawdata <- myplot$dataset$rawdata
    if (input$npage == "")
    {
      mydata <-
        DT::datatable(rawdata, options = list(orderClasses = TRUE))
    }
    else
    {
      current_page <- myplot$defs[[input$npage]]
      data_file <- current_page$data
      if (input$nforest == "")
      {
        return
      }
      else
      {
        if (is.null(myplot$dataset$data[[input$npage]]))
          getForestData()
        mydata <-
          myplot$dataset$data[[input$npage]][[input$nforest]]
        mydata <- data.frame(lapply(mydata, function(y)
          if (is.numeric(y))
            round(y, 4)
          else
            y))
        #data.frame(lapply(mydata, function(y) if(is.numeric(y)) round(y,input$decimalplaces) else y))
        DT::datatable(mydata,
                      options = list(orderClasses = TRUE))
      }
    }
    
    
    #head(mydata, n = input$obs)
  })
  
  
  output$click_info <- renderText({
    paste0("x=", input$plot_click$x, ", y=", input$plot_click$y)
  })
  output$preview <- renderPlot({
    if (is.null(input$data_file) | is.null(input$graphdef_file)) {
      print("missing data or gragh definations")
      return(NULL)
    }
    SaveForestData()
    # set up the output file
    devs <- dev.cur()
    #
    #update the reactive variables
    coldefs$dat <-
      myplot$dataset$colsdefs[[input$npage]][[input$nforest]]
    #
    trials.setpage(myplot$defs[[input$npage]])
    p <-
      trials.forest(myplot$defs[[input$npage]], grid.overlay = TRUE)
    
    print(devs)
    print(dev.cur())
    dev.copy(
      which = devs,
      width = dev.size("px")[1],
      height = dev.size("px")[2],
      res = 96,
      bg = "transparent"
    )
    dev.off(dev.prev())
    dev.size()
    
  }, width = exprToFunction(input$width * 96),
  height = exprToFunction(input$height * 96), res = 96, pointsize = 8)
  
  output$plot <- renderPlot({
    if (is.null(input$data_file) | is.null(input$graphdef_file)) {
      print("missing data or gragh definations")
      return(NULL)
    }
    # recalculate
    if (length(myplot$defs) == 0)
    {
      print("no graph defs defined")
    } else
    {
      fileName <- paste0(input$plotdata, "\\", input$filestem, "_gui.xml")
      if (file.exists(fileName))
      {
        file.copy(fileName, paste0(fileName, ".bak"), overwrite = TRUE)
      }
      gdefs <- myplot$defs[input$npage]
      gdefs[[input$npage]]["output.type"] <- "gui"
      trials.saveplotdefxml(gdefs, filename = fileName)
      cat(paste0("Graph defs file ", fileName, " has been updated or created\n"))
    }
    
    if (file.exists(input$plotdata))
    {
      file.copy(input$plotdata,
                paste0(input$plotdata, ".bak"),
                overwrite = TRUE)
      SaveForestData()
    }
    # copy the working windows
    devs <- dev.cur()
    
    trials(
      plot.def = fileName,
      plot.def.mode = "XML",
      output_folder = input$output
    )
    print(devs)
    print(dev.cur())
    dev.copy(
      which = devs,
      width = dev.size("px")[1],
      height = dev.size("px")[2],
      res = 96,
      bg = "transparent"
    )
    dev.off(dev.prev())
  }, width = exprToFunction(input$width * 96),
  height = exprToFunction(input$height * 96), res = 96, pointsize = 8)
})
