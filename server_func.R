
# read  SAS Jasper mapping file
Get_SAS_Map <- function(sas_map = "sas_map.csv")
{
  if (!file.exists(sas_map)) {
    #reporting error
    print("no sas map file found")
    return(NULL)
  }
  return(read.csv(sas_map))
}
# read  graphdef rows file file
Get_Graphdef_Rows <- function(file)
{
  if (!file.exists(file)) {
    #reporting error
    print("no graphdef rows file found")
    return(NULL)
  }
  return(read.csv(file))
}
# read  graphdef columns mapping file
Get_Graphdef_Cols <- function(file)
{
  if (!file.exists(file)) {
    #reporting error
    print("no graphdef columns file found")
    return(NULL)
  }
  return(read.csv(file))
}
# get  the data from the non-ignored columns only
Get_Non_Ignored_Data  <- function(rawdata)
{
  sas_map <- Get_SAS_Map()
  if (toString(colnames(rawdata)) != toString(sas_map[, "SAS"]))
  {
    #reporting error
    print("data column named do not match  those in the mapping file ")
    return(NULL)
  }
  return(rawdata[, sas_map[which(sas_map$Ignore != 1), 1]])
  
}

# get  forest-related columns , and map the columns
# to  Jasper defined column names
#LogRR1,LogRR_LCI1,LogRR_UCI1,Trend1
Get_Forest_Columns <- function(sas_map, index = 1)
{
  ret <- NULL
  Jasper_Columns <-
    sas_map[which(sas_map$Jasper != ""), c("SAS", "Jasper")]
  ret$SAS <- paste0(sas_map[which(sas_map$Jasper != ""), "SAS"])
  ret$Jasper <-
    paste0(sas_map[which(sas_map$Jasper != ""), "Jasper"], as.character(index))
  return(ret)
}

#rename    forest-related columns
# to  Jasper defined column names
#LogRR1,LogRR_LCI1,LogRR_UCI1,Trend1

Rename_Forest_Columns <- function(dat, sas_map, index = 1)
{
  ret <- Get_Forest_Columns(sas_map, index)
  # rename using data.table function setnames
  return(setnames(dat, ret$SAS, ret$Jasper))
}
Get_Selected_Figure  <- function(figure_list, figure_id)
{
  return(figure_list[which(figure_list$figref == figure_id), ])
}
#load data by figure ref no
Get_Data_by_Figure  <- function(dat, dat_fig, figure_id)
{
  dat_fig_selected <- dat_fig[which(dat_fig$figref == figure_id), ]
  return(merge(dat, dat_fig_selected, all.y = TRUE))
}
# remove columns with all NaN values from  a data frame
Remove_NaN_Columns <- function(dat)
{
  bad <- sapply(dat, function(x)
    all(is.nan(x)))
  return(dat[, !bad])
}
# get headings Columns ColHeadings
Get_Headings_Columns <- function(coldefs, figure_id)
{
  if (figure_id == 1)
  {
    Heading_Columns <-
      c("failed1=Active allocated", "failed2=Placbo allocated")
    
  }
  return(Heading_Columns)
}

#


# Return the requested plot size
# 96 DPI as default

getPageSize <-
  function(page_type = "a4",
           page_orientation = "portrait",ppi=96)
  {
    if (page_type == "a4")
      #a4
    {
      if (page_orientation == "landscape")
      {
        ret <- c(1122, 793)
      }
      else
        ret <- c(793, 1122)
    }
    else if (page_type == "us")
      # letter
      
    {
      if (page_orientation == "landscape")
      {
        ret <- c(1056, 816)
      }
      else
        ret <- c(816, 1065)
    } else
    {
      #ppt
      ret <- c(1024, 768)
    }
    #print(ret/ppi)
    return(round(ret/ppi,2))
  }
  # sync with the changes in tbl_coldefs
  #' calculate the selected page dateset from  data and graphdef files
  getForestData <- <- function(myplot = NULL, page_selected = NULL, data_loaded =NULL,key="disporder"){
    #skip it if no data/defs loaded
    if (length(myplot$defs) == 0 )
    {
      print(print("no graph defs defined "))
	  return()
    }
    if (is.null(myplot$defs[[page_selected]))
	   cat(paste0("Page ", page_selected, " is not defined\n"))
      return()
    current_page <- myplot$defs[[page_selected]]
    # get forest list
    forest_list <-
      names(current_page)[grepl("forest", names(current_page))]
    if (is.null(forest_list))
    {
      cat ("no forest plot defined in this selected page:",
           page_selected)
      return()
    } 
	else 
    {
	  # only TRUE if there are  more than 1 forest plot 
      multiple.forest <- length(forest_list) > 1
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
	if (is.null(cpage$rowdefs))
    {
      cat ("no rows defined in this selected page:",
           page_selected)
      return()
    }   
    
			 
    if(data_loaded)
	{
	cpage$rawdata <-
      subset(myplot$dataset$rawdata,
             disporder %in% cpage$rowdefs$disporder)		 
	}
	else
    {
	 file.name<-paste0(trimws(myplot$defs$page1$filestem),".sas7bdat")
	 if(file.exists(file.name))
	 {
	  cpage$rawdata<-sas7bdat::read.sas7bdat(file.name))
	  #save to the raw data into the variable
	  if(is.null(myplot$dataset$rawdata2))
	   {
 	   myplot$dataset$rawdata2<-list()
	   }
	  myplot$dataset$rawdata2[[myplot$defs$page1$filestem]]<-cpage$rawdata
	 cat ("Raw data is loaded from ",file.name)
	 } else
	 {
	  cat ("Raw data is  not loaded because the file ",file.name, " does not exist")
	  return()
	 }
	}
	 if(is.null(cpage$rawdata))
    {
	      cat ("no row date in this selected page:", page_selected)
		  return()
	}
    # looping  by forest
    index = 0
	cols.forest <-
        c(current_forest$estimatecol,
          current_forest$lcicol,
          current_forest$ucicol)
		  
    for (nforest in forest_list)
    {
      index = index + 1
      current_forest <- current_page[[nforest]]
	  cols.vars<-c("total1","total2","comp1","comp2")
      cols.text <-
        intersect(current_forest$columns, names(cpage$rawdata))
      cols.text.decimalplaces <-
        current_forest$columns.decimalplaces
      cols.text.type <-
        current_forest$columns.type
      
      
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
            current_row$text <-
              data.frame(matrix(NA, ncol = length(cols.text), nrow = 1))
            names(current_row$text) <- cols.text
            current_row$forest <-
              data.frame(matrix(
                NA,
                ncol = length(cols.forest),
                nrow = 1
              ))
            names(current_row$forest) <- cols.forest
			# replacing the description column with the text in the alt.description 
		   current_row$text [1] <-  current_row$alt$alt.description
			
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
            print(" a blank line is added") 
          } else if (current_rowdefs$type == "title")
          {
            current_row$text [1] <-cpage$rawdata[]
          }else
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
	  cpage$data$forest[[nforest]]$p_out[cpage$data$forest[[nforest]]$p_out<0.001]<-"<0.001"
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
  
   get.Pvalue.bounded<-function(pvalue.source,lower=0.001,upper=0.999)
{
   #		Merck formatting standards:
      #   If the value is less than 0.001 display "<0.001"
	  pvalue.output<-pvalue.source
	  pvalue.output[pvalue.output<lower]<-paste0("<",as.character(lower))
	  pvalue.output[pvalue.output>upper]<-paste0("<",as.character(upper))
	  return (pvalue.output)
}

	 
