readCrnFile <- function() {
  done <- tclVar(0)
  filenamevar <- tclVar("")
  tabnamevar <- tclVar("")
  header.var <- tclVar(0)
  show.data <- tclVar(0)
  header.bl <- FALSE

  choosefile <- function() {
    fictrt <- tkgetOpenFile()
    fpath <- tclvalue(fictrt)
    tkfocus(win)
    if (fpath != "") {
      dat <- file(fpath, "r")
      dat1 <- readLines(dat, warn = FALSE)
      tkconfigure(TXT, state = "normal")
      tkdelete(TXT, "0.0", "end")
      nLine <- paste("Line", 1:length(dat1), "= ", sep = "")

      for (i in 1:length(dat1)) {
        tkinsert(TXT, "end", paste(nLine[i], dat1[i], "\n", sep = ""))
      }
      close(dat)
      tkconfigure(TXT, state = "disabled")
      tkdelete(file.entry, 0, "end")
      tkinsert(file.entry, "end", fpath)
      tkdelete(tab.entry, 0, "end")
      tkinsert(tab.entry, "end", basename(fpath))
    }
  }

  choosefic <- function() {
    if (tclvalue(tabnamevar) != "") {
      tabname <- parse(text = tclvalue(tabnamevar))[[1]]
    } else {
      tabname <- "untitled"
    }
    if (tclvalue(filenamevar) != "") {
      filename <- tclvalue(filenamevar)
    } else {
      return()
    }
    tkdestroy(win)

    n.header <- 0
    if (!is.na(as.numeric(tclvalue(header.var)))) {
      n.header <- tclvalue(header.var)
    }
    if (n.header > 0) header.bl <- TRUE
    rdcom <- paste(tabname, " <<- readCrn(fname='", filename, "', header=", header.bl, ", n.header=", n.header, ", info=FALSE)", sep = "")
    eval(parse(text = rdcom))

    show.data.flag <- as.logical(tclObj(show.data))

    if (show.data.flag) eval(parse(text = paste("edit(", tabname, ")", sep = "")))
  }

  win <- tktoplevel()
  tkwm.title(win, "Open a crn file")
  tkwm.resizable(win, 0, 0)
  tkwm.geometry(win, paste("+0+", .get(".height"), sep = ""))
  tkwm.deiconify(win)
  tkgrab.set(win)
  tkfocus(win)

  frame1 <- tkframe(win, relief = "groove")
  frame2 <- tkframe(win, relief = "groove")
  frame.preview <- tkframe(win, relief = "groove")

  tab.entry <- tkentry(frame1, textvariable = tabnamevar)
  file.entry <- tkentry(frame1, textvariable = filenamevar)
  separator <- tklabel(frame1, text = "")
  choosefile.but <- tkbutton(frame1, text = "...", command = function() choosefile())
  tkgrid(tklabel(frame1, text = "Select a file to read: "), file.entry, separator, choosefile.but, sticky = "w")
  tkgrid(tklabel(frame1, text = "Enter name for data set: "), tab.entry, sticky = "w")
  tkpack(frame1, fill = "x")

  SRC <- tkscrollbar(frame.preview, repeatinterval = 5, command = function(...) tkyview(TXT, ...))
  SRC1 <- tkscrollbar(frame.preview, repeatinterval = 5, orient = "horizontal", command = function(...) tkxview(TXT, ...))
  TXT <- tktext(frame.preview, height = 5, width = 37, yscrollcommand = function(...) tkset(SRC, ...), xscrollcommand = function(...) tkset(SRC1, ...), background = "grey", wrap = "none")
  tkgrid(tklabel(frame.preview, text = "Preview:", foreground = "blue"), sticky = "w")
  tkgrid(TXT, SRC)
  tkgrid(SRC1)
  tkgrid.configure(SRC, sticky = "ns")
  tkgrid.configure(SRC1, sticky = "ew")
  tkpack(frame.preview)

  show.data.cbut <- tkcheckbutton(frame2, text = "Show data frame                       ", variable = show.data)

  header.lab <- tklabel(frame2, text = "Header lines: ")
  header.entry <- tkentry(frame2, textvariable = header.var, width = 2)

  tkpack(show.data.cbut, side = "left")
  tkpack(header.lab, header.entry, side = "left")
  tkpack(frame2)

  frame.exit <- tkframe(win, relief = "groove")
  fr.exit.space <- tklabel(frame.exit, text = " ")
  ok.but <- tkbutton(frame.exit, text = "      Ok      ", command = function() choosefic())
  cancel.but <- tkbutton(frame.exit, text = "    Cancel    ", command = function() tkdestroy(win))
  tkgrid(cancel.but, fr.exit.space, ok.but)
  tkpack(frame.exit)
  tkfocus(win)
  tkbind(win, "<Destroy>", function() tclvalue(done) <- 2)
  tkbind(win, "<KeyPress-Escape>", function() tkdestroy(win))
  tkbind(win, "<KeyPress-Return>", choosefic)
  tkbind(win, "<KP_Enter>", choosefic)
  tkwait.variable(done)

  if (tclvalue(done) == "2") {
    return(0)
  }
  tkdestroy(win)
}
