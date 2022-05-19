library(tcltk, lib.loc = "C:/Program Files/R/R-4.0.2/library")

abstract_screener_custom <- function (file = file.choose(), aReviewer = NULL, reviewerColumnName = "REVIEWERS",
          unscreenedColumnName = "INCLUDE", unscreenedValue = "not vetted",
          abstractColumnName = "abstract", titleColumnName = "title",
          yearColumnName = "year", journalColumnName = "journal",
          browserSearch = "https://www.google.com/search?q=", fontSize = 13,
          windowWidth = 70, windowHeight = 5, theButtons = c("YES",
                                                              "maybe", "NO"), keyBindingToButtons = c("y", "m", "n"),
          buttonSize = 10, highlightColor = "powderblue", highlightKeywords = NA)
{
  aDataFrame <- read.csv(file, header = TRUE)
  subData <- subset(aDataFrame, aDataFrame[reviewerColumnName] ==
                      aReviewer)
  subData <- data.frame(lapply(subData, as.character), stringsAsFactors = FALSE)
  if (unscreenedValue %in% subData[, unscreenedColumnName]) {
    currentItem <- max.col(t(subData[unscreenedColumnName] ==
                               unscreenedValue), "first")
  }
  else {
    .metagearPROBLEM("error", paste("all abstracts have already been screened,\n                           no more abstracts coded as:",
                                    unscreenedValue))
  }
  if (requireNamespace("tcltk", quietly = TRUE)) {
    gsubTEXT <- paste0("(.{1,", windowWidth + 10, "})(\\s|$)")
    insert_tktext <- function(theTextWidget, someText, textFormat,
                              highlightKeywords, highlightColor, refresh = FALSE) {
      newText <- paste0("  ", gsub(textFormat, "\\1\n  ",
                                   someText))
      if (refresh == TRUE)
        tkdelete(theTextWidget, "1.0", "end")
      else tkinsert(theTextWidget, "1.0", newText)
      if (anyNA(highlightKeywords)) {
        tkfocus(screenerWindow)
        return()
      }
      theIndex <- 0
      thePos <- stringr::str_locate_all(pattern = paste(highlightKeywords,
                                                        collapse = "|"), unlist(strsplit(newText, "\n")))
      for (i in thePos) {
        theIndex <- theIndex + 1
        if (length(i) != 0) {
          for (j in 1:(length(i)/2)) {
            if (refresh == TRUE) {
              tktag.delete(theTextWidget, paste0("aTag",
                                                 theIndex))
            }
            else {
              tktag.add(theTextWidget, paste0("aTag",
                                              theIndex), paste0(theIndex, ".", i[j,
                                                                                 1] - 1), paste0(theIndex, ".", i[j,
                                                                                                                  2]))
              tktag.configure(theTextWidget, paste0("aTag",
                                                    theIndex), background = highlightColor)
            }
          }
        }
      }
      tkfocus(screenerWindow)
      return()
    }
    refresh_text <- function() {
      insert_tktext(titleText, subData[currentItem, titleColumnName],
                    gsubTEXT, highlightKeywords, highlightColor,
                    refresh = TRUE)
      insert_tktext(titleText, subData[currentItem, titleColumnName],
                    gsubTEXT, highlightKeywords, highlightColor)

      insert_tktext(yearText, subData[currentItem, yearColumnName],
                    gsubTEXT, highlightKeywords, highlightColor,
                    refresh = TRUE)
      insert_tktext(yearText, subData[currentItem, yearColumnName],
                    gsubTEXT, highlightKeywords, highlightColor)

      insert_tktext(journalText, subData[currentItem, journalColumnName],
                    gsubTEXT, highlightKeywords, highlightColor,
                    refresh = TRUE)
      insert_tktext(journalText, subData[currentItem, journalColumnName],
                    gsubTEXT, highlightKeywords, highlightColor)


      insert_tktext(abstractText, subData[currentItem,
                                          abstractColumnName], gsubTEXT, highlightKeywords,
                    highlightColor, refresh = TRUE)
      insert_tktext(abstractText, subData[currentItem,
                                          abstractColumnName], gsubTEXT, highlightKeywords,
                    highlightColor)
      return()
    }
    confirmDialog <- function(theValue, oldValue) {
      updateTheValue <- tkmessageBox(type = "yesno", icon = "warning",
                                     title = "Warning", message = paste0("Previously screened as: ",
                                                                         oldValue, "\rDo you wish to update to: ",
                                                                         theValue))
      if (as.character(updateTheValue) == "yes")
        updateAll(theValue)
      tkfocus(screenerWindow)
      return()
    }
    statusDialog <- function() {
      tkmessageBox(title = "Current Abstract Status",
                   icon = "info", message = paste0("SCREENING OUTCOME: ",
                                                   subData[currentItem, unscreenedColumnName],
                                                   "."))
      tkfocus(screenerWindow)
      return()
    }
    refreshDialog <- function() {
      refresh_text()
      return()
    }
    for (i in theButtons) {
      eval(parse(text = paste0("theAnswer", i, " <- function() {\n",
                               "if(subData[currentItem, unscreenedColumnName] != unscreenedValue) {\n",
                               "confirmDialog(\"", i, "\", oldValue = subData[currentItem, unscreenedColumnName])\n",
                               "} else {\n", "updateAll(\"", i, "\")\n", "}\n",
                               "tkfocus(screenerWindow)\n", "}")))
    }
    updateAll <- function(theValue) {
      if (currentItem <= nrow(subData)) {
        subData[[currentItem, unscreenedColumnName]] <<- theValue
        currentItem <<- currentItem + 1
      }
      if (currentItem > nrow(subData)) {
        insert_tktext(abstractText, subData[currentItem,
                                            abstractColumnName], gsubTEXT, highlightKeywords,
                      highlightColor, refresh = TRUE)
        insert_tktext(abstractText, "You have screened all the Abstracts!",
                      gsubTEXT, highlightKeywords, highlightColor)
        insert_tktext(titleText, subData[currentItem,
                                         titleColumnName], gsubTEXT, highlightKeywords,
                      highlightColor, refresh = TRUE)
      }
      else {
        refresh_text()
        tclvalue(theProgress) <- paste0("Reviewer: ",
                                        aReviewer, "\n", round(((currentItem - 1)/nrow(subData)) *
                                                                 100, digits = 1), "% complete (", currentItem,
                                        " of ", nrow(subData), ")")
      }
    }
    backtrack <- function() {
      if (currentItem != 1) {
        currentItem <<- currentItem - 1
        refresh_text()
        tclvalue(theProgress) <- paste0("Reviewer: ",
                                        aReviewer, "\n", round(((currentItem - 1)/nrow(subData)) *
                                                                 100, digits = 1), "% complete (", currentItem,
                                        " of ", nrow(subData), ")")
      }
    }
    searchBrowser <- function() browseURL(paste0(browserSearch,
                                                 subData[currentItem, titleColumnName]))
    saveProgress <- function() {
      write.csv(subData, file = file, row.names = FALSE)
      tclvalue(theSaveState) <- paste("last saved: ",
                                      Sys.time())
    }
    screenerWindow <- tktoplevel()
    tktitle(screenerWindow) <- "metagear: Abstract Screener"
    theTitle <- subData[currentItem, titleColumnName]
    theYear <- subData[currentItem, yearColumnName]
    theJournal <- subData[currentItem, journalColumnName]
    theAbstract <- as.character(subData[currentItem, abstractColumnName])
    theProgress <- tclVar(paste0("Reviewer: ", aReviewer,
                                 "\n", round(((currentItem - 1)/nrow(subData)) *
                                               100, digits = 1), "% complete (", currentItem,
                                 " of ", nrow(subData), ")"))
    theSaveState <- tclVar(paste0("last saved: never"))
    screenerMenu <- tkmenu(screenerWindow)
    tkconfigure(screenerWindow, menu = screenerMenu)
    file_menu <- tkmenu(screenerMenu)
    tkadd(screenerMenu, "cascade", label = "Issue Fixes",
          menu = file_menu)
    tkadd(file_menu, "command", label = "Refresh Title and Abstract Text",
          command = refreshDialog)
    tkadd(file_menu, "command", label = "Status of Current Abstract",
          command = statusDialog)
    tkadd(file_menu, "command", label = "Return to Previous Abstract",
          command = backtrack)
    titleFrame <- ttklabelframe(screenerWindow, text = "Title",
                                padding = 5)
    titleText <- tktext(titleFrame, font = tkfont.create(size = fontSize),
                        height = 3, width = windowWidth)
    insert_tktext(titleText, theTitle, gsubTEXT, highlightKeywords,
                  highlightColor)
    searchButton <- ttkbutton(titleFrame, text = "\nSearch\n  Web\n",
                              command = searchBrowser, width = 10)
    tkgrid(titleText, searchButton, padx = 5)
    # year
    yearFrame <- ttklabelframe(screenerWindow, text = "Year",
                                padding = 5)
    yearText <- tktext(yearFrame, font = tkfont.create(size = fontSize),
                        height = 3, width = windowWidth)
    insert_tktext(yearText, theYear, gsubTEXT, highlightKeywords,
                  highlightColor)
    searchButton <- ttkbutton(yearFrame, text = "\nSearch\n  Web\n",
                              command = searchBrowser, width = 10)


    tkgrid(yearText,padx = 5)
    #

    # journal
    journalFrame <- ttklabelframe(screenerWindow, text = "Journal",
                               padding = 2)
    journalText <- tktext(journalFrame, font = tkfont.create(size = fontSize),
                       height = 3, width = windowWidth)
    insert_tktext(journalText, theJournal, gsubTEXT, highlightKeywords,
                  highlightColor)
    searchButton <- ttkbutton(journalFrame, text = "\nSearch\n  Web\n",
                             command = searchBrowser, width = 10)

    tkgrid(journalText, padx = 5)
    #


    abstractFrame <- ttklabelframe(screenerWindow, text = "Abstract",
                                   padding = 10)
    abstractScroll <- tkscrollbar(screenerWindow, orient = "vertical",
                                  command = function(...) tkyview(abstractText, ...))
    abstractText <- tktext(abstractFrame, font = tkfont.create(size = fontSize),
                           height = windowHeight, width = windowWidth + 7,
                           yscrollcommand = function(...) tkset(abstractScroll,
                                                                ...))
    insert_tktext(abstractText, theAbstract, gsubTEXT, highlightKeywords,
                  highlightColor)
    tkgrid(abstractText, abstractScroll, sticky = "nsew")
    selectionFrame <- ttklabelframe(screenerWindow, text = "Is relevant?",
                                    padding = 10)
    for (i in theButtons) eval(parse(text = paste0(i, "Button <- ttkbutton(selectionFrame, text = \"\n",
                                                   i, "\n\"", ", command = theAnswer", i, ", width = buttonSize)")))
    for (i in 1:length(theButtons)) eval(parse(text = paste0("tkbind(screenerWindow, \"<",
                                                             keyBindingToButtons[i], ">\", theAnswer", theButtons[i],
                                                             ")")))
    eval(parse(text = paste0("tkgrid(", paste0(theButtons,
                                               "Button", collapse = ", "), ", padx = 5)")))
    eval(parse(text = paste0("selectionLabel <- ttklabel(selectionFrame, text = \"\nNote: You can also press ",
                             paste0("'", head(keyBindingToButtons, -1), "'",
                                    collapse = ", "), ", or ", "'", tail(keyBindingToButtons,
                                                                         1), "' on keyboard.\", font = tkfont.create(size = 7))\n")))
    eval(parse(text = paste0("tkgrid(selectionLabel, columnspan = 3)")))
    progressFrame <- ttklabelframe(screenerWindow, text = "Progress",
                                   padding = 10)
    progressLabel <- ttklabel(progressFrame, textvariable = theProgress)
    saveProgressButton <- ttkbutton(progressFrame, text = "Save",
                                    command = saveProgress, width = 10)
    lastSaveLabel <- ttklabel(progressFrame, textvariable = theSaveState)
    tkgrid(progressLabel, saveProgressButton, lastSaveLabel,
           padx = 5)
     tkpack(titleFrame, yearFrame, side = "top", pady = 10,
            padx = 10)
     tkpack(yearFrame, journalFrame, side = "top", pady = 10,
            padx = 10)
     tkpack(journalFrame, abstractFrame, side = "top", pady = 10,
            padx = 10)

    tkpack(selectionFrame, side = "left", pady = 10, padx = 10)
    tkpack(progressFrame, fill = "x", side = "right", pady = 5,
           padx = 5)
    tkfocus(screenerWindow)
  }
  else {
    .metagearPROBLEM("error", paste("\n tcltk package is missing and is needed to generate the abstract screener GUI.",
                                    "  --> If using Windows/Linux, try 'install.packages('tcltk')'",
                                    "  --> If using a Mac, install latest XQuartz application (X11) from:",
                                    "        https://www.xquartz.org/", sep = "\n"))
  }
}



# # Load package
# library(metagear)
#
# # Load Data
# data(example_references_metagear)
#
#
#
# # Illustration of the custom widget with Year and Journal added
# abstract_screener_custom("effort_marc.csv",
#                          aReviewer = "marc",
#                          highlightKeywords = "and")
#


