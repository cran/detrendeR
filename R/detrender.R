
getSelectDataset <- function() {
  select_dataset <- .get("DataBaseChoice")
  if (exists(select_dataset, envir = .GlobalEnv)) {
    select_dataset <- get(select_dataset, envir = .GlobalEnv)
  }
  select_dataset
}

saveCompact <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    fname <- tclvalue(
      tkgetSaveFile(
        initialfile = DataBaseChoice,
        defaultextension = ".cpt",
        filetypes = " {{All Files} {*.*}} {CPT {.cpt}}"
      )
    )
    if (fname != "") {
      try(write.rwl(getSelectDataset(), fname = fname, format = "compact"),
        silent = TRUE
      )
    }
  }
}

saveCsv <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    fname <- tclvalue(
      tkgetSaveFile(
        initialfile = DataBaseChoice,
        defaultextension = ".csv",
        filetypes = "{CSV {.csv}} {{All Files} {*.*}} "
      )
    )
    if (fname != "") {
      # TODO
      try(eval(parse(text = paste(
        "YEAR<-row.names(",
        DataBaseChoice, ")"
      ))), silent = TRUE)
      # TODO
      TempDataBase <- NULL
      try(eval(parse(
        text = paste(
          "TempDataBase <- data.frame(YEAR,",
          DataBaseChoice, ")"
        )
      )), silent = TRUE)
      try(write.table(
        TempDataBase,
        file = fname,
        quote = FALSE,
        sep = ";",
        na = "",
        row.names = FALSE
      ))
    }
  }
}

saveRwl01 <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    fname <- tclvalue(
      tkgetSaveFile(
        initialfile = DataBaseChoice,
        defaultextension = ".rwl",
        filetypes = "{RWL {.rwl}} {{All Files} {*.*}} "
      )
    )
    if (fname != "") {
      try(write.rwl(getSelectDataset(),
        fname = fname,
        long.names = TRUE
      ),
      silent = TRUE
      )
    }
  }
}

saveRwl001 <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    fname <- tclvalue(
      tkgetSaveFile(
        initialfile = DataBaseChoice,
        defaultextension = ".rwl",
        filetypes = " {RWL {.rwl}} {{All Files} {*.*}}"
      )
    )
    if (fname != "") {
      try(write.rwl(
        getSelectDataset(),
        fname = fname,
        prec = 0.001,
        long.names = TRUE
      ),
      silent = TRUE
      )
    }
  }
}

saveCrn <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    fname <- tclvalue(tkgetSaveFile(
      initialfile = DataBaseChoice,
      defaultextension = ".crn", filetypes = " {{All Files} {*.*}} {CRN {.crn}}"
    ))
    # .assign("fname", tclvalue(tkgetSaveFile(
    #   initialfile = DataBaseChoice,
    #   defaultextension = ".crn", filetypes = " {{All Files} {*.*}} {CRN {.crn}}"
    # )))
    if (fname != "") {
      # TODO
      try(eval(parse(text = paste(
        "write.crn(",
        DataBaseChoice, "[,1:2], fname = fname)"
      ))),
      silent = T
      )
    }
  }
}

changeDir <- function() {
  try(setwd(.tcl2str(tkchooseDirectory())), silent = TRUE)
}

saveWorkspace <- function() {
  try(save.image(tclvalue(tkgetSaveFile(
    title = "Save image...",
    defaultextension = ".RData", filetypes = " {{All Files} {*.*}} {R_Image {.RData}}"
  ))),
  silent = TRUE
  )
}

callArstan <- function() {
  flag <- detrenderGUI()
  try(if (flag) {
    ARSTAN()
  }, silent = TRUE)
}


exitR <- function() {
  response <- tclvalue(tkmessageBox(
    message = "Save workspace image?",
    type = "yesnocancel", title = "Question", icon = "question"
  ))
  if (response == "cancel") {
    return(invisible(response))
  }
  if (response == "yes") {
    try(save.image(tclvalue(tkgetSaveFile(
      title = "Save image...",
      defaultextension = ".RData", filetypes = " {{All Files} {*.*}} {R_Image {.RData}}"
    ))),
    silent = T
    )
    return(invisible(q(save = "no")))
  }
  if (response == "no") {
    return(invisible(q(save = "no")))
  }
}

closeGUI <- function() {
  tkdestroy(.get("GUI"))
}




export2GlobalEnv <- function(name, obj, env = .GlobalEnv) {
  assign(name, obj, env)
}


SERIES_INFORMATION <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    cat("\n[", DataBaseChoice, "]\n", sep = "")
    try(seriesInfo(getSelectDataset()), silent = TRUE)
  }
}


TREE_IDS <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    stc <- .get("stc")
    cat("\n[", DataBaseChoice, "]", sep = "")
    # TODO
    a <- try(eval(parse(
      text = paste(
        "TreeIds(",
        DataBaseChoice,
        ", stc=c(",
        stc[1],
        ",",
        stc[2],
        ",",
        stc[3],
        "))"
      )
    )), silent = TRUE)
  }
}



MISSING_RINGS <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    cat("\n[", DataBaseChoice, "]", sep = "")
    a <- try(eval(parse(text = paste(
      "TrwLessThan(",
      DataBaseChoice, ",TRW=0)"
    ))), silent = TRUE)
  }
}


RWL_INFO <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    cat("\n[", DataBaseChoice, "]\n", sep = "")
    a <- try(eval(parse(text = paste(
      "RwlInfo(", DataBaseChoice,
      ")"
    ))), silent = TRUE)
  }
}



SEG_PLOT <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    op <- par(oma = c(0, 0, .5, 0))
    on.exit(par(op))
    try(eval(parse(
      text = paste0(
        "seg.plot(",
        DataBaseChoice,
        "); title(main='", DataBaseChoice, "', outer=TRUE, line=-0.5)"
      )
    )),
    silent = TRUE
    )
  }
}


RWL_PLOT <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    a <- try(eval(parse(
      text = paste(
        "plotRwl(",
        DataBaseChoice,
        ", main='",
        paste(DataBaseChoice),
        "', save.csv=FALSE)"
      )
    )),
    silent = TRUE
    )
  }
}

makeCRONO <- function() {
  if (length(listDataSets()) == 0) {
    return()
  }
  tk_chrono(input = .get("DataBaseChoice"))
  tkfocus(.get("GUI"))
}

AR.MODEL <- function() {
  if (length(listDataSets()) == 0) {
    return()
  }
  arMODEL(input = .get("DataBaseChoice"))
}

EPS <- function() {
  if (length(listDataSets()) == 0) {
    return()
  }
  interactiveEPS(input = .get("DataBaseChoice"))
}


DETRENDING_1STEP <- function() {
  if (length(listDataSets()) == 0) {
    return()
  }
  .assign("TwoSteps", FALSE)
  detrending(TwoSteps = FALSE, input = .get("DataBaseChoice"))
}

DETRENDING_2STEP <- function() {
  if (length(listDataSets()) == 0) {
    return()
  }
  .assign("TwoSteps", TRUE)
  detrending(TwoSteps = TRUE, input = .get("DataBaseChoice"))
}

update_DataBaseSelectedBt <- function() {
  DataBases <- listDataSets()
  DataBaseSelectedBt <- .get("DataBaseSelectedBt")
  if (length(DataBases) == 0) {
    DataBaseChoice <- c("<No active dataset>")
    .assign("DataBaseChoice", DataBaseChoice)

    tkconfigure(DataBaseSelectedBt,
      textvariable = tclVar(DataBaseChoice),
      bg = "red", foreground = "black"
    )
    tkfocus(.get("GUI"))
    return()
  }

  DataBaseChoice <- tk_select.list(sort(listDataSets()),
    title = "Select one"
  )
  .assign("DataBaseChoice", DataBaseChoice)
  if (DataBaseChoice != "") {
    tkconfigure(DataBaseSelectedBt,
      textvariable = tclVar(DataBaseChoice),
      background = "grey80", foreground = "blue"
    )
  }
  if (DataBaseChoice == "") {
    DataBaseChoice <- c("<No active dataset>")
    .assign("DataBaseChoice", DataBaseChoice)
    tkconfigure(DataBaseSelectedBt,
      textvariable = tclVar(DataBaseChoice),
      bg = "red", foreground = "black"
    )
  }
  tkfocus(.get("GUI"))
}

DELETE_DATASET <- function() {
  DataBaseChoice <- .get("DataBaseChoice")
  if (DataBaseChoice != "<No active dataset>") {
    DatasetsToDelete <- tk_select.list(listDataSets(),
      preselect = DataBaseChoice, multiple = TRUE,
      title = "Select datasets to delete"
    )
    if (length(DatasetsToDelete) > 0) {
      for (i in 1:length(DatasetsToDelete)) {
        a <- try(eval(parse(text = paste(
          "rm(",
          DatasetsToDelete[i], ",envir = .GlobalEnv)"
        ))),
        silent = TRUE
        )
      }
      if (any(DatasetsToDelete == DataBaseChoice)) {
        DataBaseChoice <- c("<No active dataset>")
        .assign("DataBaseChoice", DataBaseChoice)
        DataBaseSelectedBt <- .get("DataBaseSelectedBt")
        tkconfigure(DataBaseSelectedBt,
          textvariable = tclVar(DataBaseChoice),
          bg = "red", foreground = "black"
        )
      }
    }
    tkfocus(.get("GUI"))
  }
}



listDataSets <- function(envir = .GlobalEnv, ...) {
  Vars <- ls(envir = envir, all.names = TRUE)
  if (length(Vars) == 0) {
    return(Vars)
  }
  out <- names(which(sapply(Vars, function(.x) {
    is.data.frame(get(.x,
      envir = envir
    )) || is.matrix(get(.x, envir = envir))
  })))
  out
}

#' @export
#' @title detrender
#' @description Start a Graphical User Interface to perform tree-ring analyses.
#' @usage detrender()
#' @details
#' This function starts a Graphical User Interface (GUI). The window of the detrendeR GUI contains the menu bar, and three toolbars below, each comprising several buttons. The three toolbars, from up to down, are aimed at: i) managing the data set of tree-ring series to be processed; ii) provide general information about the active data set; iii) apply the statistical functions related to detrending and chronology building. The order of the different bars corresponds to the consecutive steps the user should follow to build a chronology.
#'
#' The File menu provides functions to read and save files, to exit detrendeR and to Quit R: "Read file" allows to read a data file and store the information in a new dataset. The option "clipboard" can be used to read a data table just copied from a spreadsheet program (like Excel). The items "Read rwl" and "Read crn" open a dialog box that allows to read data files in the Tucson measurement (*.rwl) and in the Tucson chronology format (*.crn). The items "Save rwl", "Save crn" and "Save csv" allows to save the active dataset into different formats (*.rwl, *.crn, *.csv). "Save Workspace..." saves the current workspace to the specified file. The saved objects can be read back from the file later by using the function load. "Quit detrendeR" closes the detrendeR window. "Quit R" opens a dialog box to ask if the environment should be saved before terminate the current R session.
#'
#' Using the File menu, datasets can be opened from different files, and a large variety of file types are supported, including Tucson measurement (*.rwl) and Tucson chronology format (*.crn). To read a data file or data from the clipboard into R, select File -> Read file.  The default name of the dataset is the name of the file to be opened, but the user is allowed to change it. In R the names of datasets must start with a letter and consist entirely of letters, digits, periods (.) and underscores (_). The user should also remember that R is case-sensitive and embedded blanks are not allowed in a dataset name.  The active dataset can also be saved into several formats (*.rwl, *.crn, *.csv) using the items Save rwl, Save crn and Save csv in the File menu.
#'
#' The Tools menu allows to define settings and to launch detrendeR in batch mode: "Define settings" brings up a dialog box, where the user can use this dialog box to define tree mask, type of detrending and other settings. "Batch mode" releases a dialog box identical to the one used to define settings, but in this case the user can choose the files to be processed by pressing the button "Ok".
#'
#' The active data set is shown in the first toolbar, immediately below the menu bar. This bar has two buttons, the first indicating the name of the active dataset, or the label "<Please select a dataset>" or "<No active dataset>" if no dataset is selected. The user can load several datasets into memory, and change the active dataset just by clicking the flat button with the active dataset name. However, at any given time, only one dataset is active. Once selected, the subsequent functions are only applied to the active dataset. The second button, Delete, allows the user to remove the active dataset and/or other datasets from the R environment.
#'
#' The second toolbar contains four buttons related to general information about the active dataset. The "Information" button displays the series identification, first and last year, and length of the series. The button "TreeIds" provides the tree mask of the active dataset. "Missing rings" indicates the existence of missing rings within the series. The last button "RwlInfo" computes some common descriptive statistics on individual series, such as the correlation with the master chronology, mean, median, standard deviation, mean sensitivity and first-order autocorrelation, and prints them to the R editor window, using the RwlInfo function.
#'
#' The lowest toolbar constitutes the core of the program, and contains four buttons that brings up different dialog boxes providing a variety of mathematical and statistical functions for trend removal, chronology building and measuring the quality of a chronology.
#'
#' The button "Detrending" extends a drop-down menu with two additional commands: "1 step" and "2 steps", that open the Detrending options dialog box window. In the first case, a single detrending method is applied to the selected dataset and two new datasets are added to the R environment, the sufixes ".cv1" and ".in1" are placed after the name of the original dataset to identify the curve and the index datasets, respectively. In the second case, four new datasets are produced by a two-step detrending, having the suffixes ".cv1", ".cv2", ".in1" and ".in2".
#'
#' detrendeR provides four different detrending methods: modified negative exponential, cubic smoothing spline, simple linear regression and through the mean. The spline algorithm used was the Andrew Bunn's fsscap function from the dplR package (Bunn, 2008). In the dialog box window "Detrending options" the checkbox "Interactive detrending" allows the user to verify how well the detrending curve fits each series, and use different methods for different series. The smoothing spline has the parameter 'bandwidth' to modify the trend elimination. Large bandwidths lead to a stiff trend line while a small bandwidth adapts smoothly to the time series, the effect of different bandwidth can be easily observed by applying the interactive detrending.
#'
#' The interactive detrending window can be closed at any time by pressing the button "Close without saving" or the button "Close and Save changes". The second detrending options have the same options as explained for the first detrending. The statistics of the detrended series are displayed in the console, by applying the RwlInfo function to the index series.
#'
#' The button "AR model" can be used to remove the autocorrelation from each series, using the R function ar.  The maximum order to be applied during the univariate autoregressive process is chosen by the user, but the selected order for each series is determined by the first minimum Akaike Information Criterion. The resulting series without persistence will be stored into a new dataset, having the suffix ".res" placed after the name of the input dataset by default.
#'
#' The button "Chrono" combines tree-ring index or raw-width series into a mean value chronology by averaging each year using an arithmetic mean or a biweight robust estimate of the mean. If the series used to produce the chronology are detrended (standardized) a Standard chronology will be produced. A Residual or a Prewhitened chronology is produced when the averaged series are residuals from the autoregressive model of the detrended series. Usually, this chronology shows a strong signal without persistence, however if some persistence remains, an autoregressive model can be applied to remove it.
#'
#' The last button, "EPS", produces several statistics that indicate the common signal to all series using the mean correlation between trees (rbt) and the Expressed Population Signal (EPS). By pressing this button the EPS analysis dialog box is launched and allows performing three analyses simultaneously, using the EPS.value function, a changed version of the rwi.stats function from the dplR package (Bunn, 2008). This function provides a variety of statistics, such as the mean within- and between-tree correlation (rwt, rbt) and the EPS (for a better explanation of the algorithm see Cook et al., 1990). In the Common interval analysis only the period where all series are represented are used to calculate the rbt and EPS values. The user can choose a certain time span and determine the rbt and EPS values for that period. The output is printed in the R console editor. The analysis can also be performed for a specified length ("Window length") and slide this window with regular steps ("Lag").
#'
#' There are two ways to end the detrendeR session. The user can select File -> Quit detrendeR and will be asked whether the R workspace should be saved. The R session will be kept working and the detrendeR can be started later by writing the command detrender() in the R console editor. The user can also select File -> Quit R and, in this case, the program will ask whether to save the R workspace (i.e., the data that R keeps in memory). This allows the user to maintain different saved workspaces for different projects.
#'
#' @references
#' Bunn, A. 2008. A dendrochronology program library in R (dplR). Dendrochronologia 26:115-124.
#'
#' Bunn, A. 2010. Statistical and visual crossdating in R using library. Dendrochronologia 28: 251-258.
#'
#' Cook, E.R., Kairiukstis, L.A. 1990. Methods of Dendrochronology: applications in the environmental sciences. Kluwer Academic Publishers.
#'
#' Fox, J. 2005. The R Commander: A basic-statistics graphical user interface to R. Journal of Statistical Software 14:142.
#'
#' Thioulouse, J., Dray, S. 2009. ade4TkGUI: ade4 Tcl/Tk Graphical User Interface. R package version 0.25. http://CRAN.R-project.org/package=ade4TkGUI.
#'
#' @examples
#' \dontrun{
#'
#' detrender()
#' }
#'
detrender <- function() {
  detrendeRversion <- "detrendeR 1.0.5"
  GUI <- .get("GUI")
  try(tkdestroy(GUI), silent = TRUE)
  .optionsDefault()
  .assign("GUI", GUI <- tktoplevel())
  tkwm.title(GUI, detrendeRversion)
  tkwm.geometry(GUI, "+0+0")

  #### frames ####
  frame0 <- tkframe(GUI, relief = "groove", borderwidth = 2)
  frame1 <- tkframe(GUI, relief = "groove", borderwidth = 2)
  frame2 <- tkframe(GUI, relief = "groove", borderwidth = 2)
  frame3 <- tkframe(GUI, relief = "groove", borderwidth = 2)

  #### frame0 ####
  topMenuFile <- tkmenubutton(frame0, text = "File")
  fileMenu <- tkmenu(topMenuFile, tearoff = FALSE)
  tkconfigure(topMenuFile, menu = fileMenu)
  tkadd(fileMenu, "command", label = "Read compact", command = readCompact)
  tkadd(fileMenu, "command", label = "Read file", command = readTable)
  tkadd(fileMenu, "command", label = "Read rwl", command = readRwlFile)

  tkadd(fileMenu, "command", label = "Read crn", command = readCrnFile)
  tkadd(fileMenu, "separator")
  tkadd(fileMenu, "command", label = "Save compact", command = saveCompact)
  tkadd(fileMenu, "command", label = "Save csv", command = saveCsv)

  saveRwlMenu <- tkmenu(fileMenu, tearoff = FALSE)
  tkadd(saveRwlMenu, "command", label = "[0.01]", command = saveRwl01)
  tkadd(saveRwlMenu, "command", label = "[0.001]", command = saveRwl001)
  tkadd(fileMenu, "cascade", label = "Save rwl", menu = saveRwlMenu)
  tkadd(fileMenu, "command", label = "Save crn", command = saveCrn)
  tkadd(fileMenu, "separator")
  tkadd(fileMenu, "command", label = "Change dir...", command = changeDir)
  tkadd(fileMenu, "separator")
  tkadd(fileMenu, "command", label = "Save Workspace...", command = saveWorkspace)

  tkadd(fileMenu, "separator")
  tkadd(fileMenu, "command", label = "Quit DetrendeR", command = closeGUI)
  tkadd(fileMenu, "command", label = "Quit R", command = exitR)

  topMenuTools <- tkmenubutton(frame0, text = "Tools")
  WinTools <- tkmenu(topMenuTools, tearoff = FALSE)
  tkconfigure(topMenuTools, menu = WinTools)
  tkadd(WinTools, "command", label = "Define settings", command = detrenderGUI)
  tkadd(WinTools, "command", label = "Batch mode", command = callArstan)
  tkpack(topMenuFile, topMenuTools, side = "left")
  tkpack(frame0, fill = "x")

  #### frame1 ####
  DataBaseSelectedBt <- tkbutton(frame1,
    text = "<Please select a dataset>",
    command = update_DataBaseSelectedBt, width = 25, height = 1, background = "grey90",
    foreground = "black"
  )
  DeleteDatasetBt <- tkbutton(frame1, text = " Delete ", command = DELETE_DATASET)
  .assign("DataBaseChoice", c("<No active dataset>"))
  .assign("DataBaseSelectedBt", DataBaseSelectedBt)
  tkgrid(DataBaseSelectedBt, DeleteDatasetBt, sticky = "w")
  tkpack(frame1, fill = "x")

  #### frame2 #####
  Information.but <-
    tkbutton(frame2, text = " Information ", command = SERIES_INFORMATION)
  TreeIds.but <-
    tkbutton(frame2, text = " TreeIds ", command = TREE_IDS)
  MISSING_RINGS.but <- tkbutton(frame2,
    text = "Missing rings ",
    command = MISSING_RINGS
  )
  RWL_INFO.but <-
    tkbutton(frame2, text = " RwlInfo ", command = RWL_INFO)
  SEG_PLOT.but <-
    tkbutton(frame2, text = " Segment plot ", command = SEG_PLOT)

  RWL_PLOT.but <- tkbutton(frame2, text = " Plot ", command = RWL_PLOT)
  tkgrid(
    Information.but, TreeIds.but, MISSING_RINGS.but, RWL_INFO.but,
    SEG_PLOT.but, RWL_PLOT.but
  )
  tkpack(frame2, fill = "x")

  #### frame3 #####
  frame3.1 <- tkframe(frame3, relief = "groove", borderwidth = 1)
  frame3.2 <- tkframe(frame3, relief = "groove", borderwidth = 0)

  topMenuDetrending <- tkmenubutton(frame3.1, text = "Detrending ")
  detrendingMenu <- tkmenu(topMenuDetrending, tearoff = FALSE)
  tkconfigure(topMenuDetrending, menu = detrendingMenu)

  tkadd(detrendingMenu, "command", label = "1 step", command = DETRENDING_1STEP)
  tkadd(detrendingMenu, "command", label = "2 steps", command = DETRENDING_2STEP)

  AR.but <- tkbutton(frame3.2, text = " AR model ", command = AR.MODEL)

  CRONObut <- tkbutton(frame3.2,
    text = " Chrono ",
    command = makeCRONO
  )

  EPSbut <- tkbutton(frame3.2, text = " EPS ", command = EPS)
  tkpack(topMenuDetrending)
  tkpack(frame3.1, side = "left")
  tkpack(AR.but, CRONObut, EPSbut, side = "left")
  tkpack(frame3.2, side = "left")
  tkpack(frame3, fill = "x")
  Sys.sleep(0.005)
  tkwm.resizable(GUI, 0, 0)

  .Tcl("update idletasks")
  tkwm.deiconify(GUI)
  tkfocus(GUI)
  return(invisible())
}
