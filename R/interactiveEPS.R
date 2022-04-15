interactiveEPS <- function(input = "") {
  run.win.analysis <- .get("run.win.analysis")
  make.common.EPS <- .get("make.common.EPS")
  winLength <- .get("winLength")
  make.select.period.EPS <- .get("make.select.period.EPS")
  first.year.common <- .get("first.year.common")
  last.year.common <- .get("last.year.common")
  stepWin <- .get("stepWin")

  if (input == "<No active dataset>") {
    input <- ""
  }
  filenamevar <- tclVar(input)
  output <- ""
  if (input != "") {
    output <- paste(input, "EPS", sep = ".")
  }
  done <- tclVar(3)
  tabnamevar <- tclVar(output)

  choose.data <- function() {
    input <- tk_select.list(sort(listDataSets()), title = "Select one")
    output <- paste(input, "EPS", sep = ".")
    tkgrab.set(tf)
    if (input != "") {
      tkdelete(file.entry, 0, "end")
      tkinsert(file.entry, "end", input)
      tkdelete(tab.entry, 0, "end")
      tkinsert(tab.entry, "end", output)
      tkfocus(tf)
    }
  }


  tf <- tktoplevel()
  tkwm.geometry(tf, paste("+0+", .get(".height"), sep = ""))
  tkwm.title(tf, "EPS analysis")
  tkwm.resizable(tf, 0, 0)
  tkwm.deiconify(tf)
  tkgrab.set(tf)
  tkfocus(tf)
  done <- tclVar(0)

  frame1.a <- tkframe(tf, relief = "groove")
  frame1 <- tkframe(tf, relief = "groove")

  tkgrid(tklabel(frame1.a, text = "Options:", foreground = "blue"))
  tkpack(frame1.a, fill = "x")

  tab.entry <- tkentry(frame1, textvariable = tabnamevar)
  file.entry <- tkentry(frame1, textvariable = filenamevar)
  choosefile.but <-
    tkbutton(frame1, text = "...", command = choose.data)
  tkgrid(
    tklabel(frame1, text = "Input name: "),
    file.entry,
    tklabel(frame1, text = " "),
    choosefile.but,
    sticky = "w"
  )
  tkgrid(tklabel(frame1, text = "Output name:"), tab.entry, sticky = "w")
  tkpack(frame1, fill = "x")

  frame4 <- tkframe(tf, relief = "groove", borderwidth = 2)
  frame4.1 <- tkframe(frame4, relief = "groove", borderwidth = 2)
  frame4.1.1 <- tkframe(frame4.1, relief = "groove", borderwidth = 0)
  frame4.1.2 <- tkframe(frame4.1, relief = "groove", borderwidth = 0)
  frame4.1.3 <- tkframe(frame4.1, relief = "groove", borderwidth = 0)

  frame4.2 <- tkframe(frame4, relief = "groove", borderwidth = 2)
  frame4.2.1 <- tkframe(frame4.2, relief = "groove", borderwidth = 0)
  frame4.2.2 <- tkframe(frame4.2, relief = "groove", borderwidth = 0)
  frame4.2.3 <- tkframe(frame4.2, relief = "groove", borderwidth = 0)

  run.win.analysis.value <- tclVar(run.win.analysis)
  winLength.value <- tclVar(winLength)
  stepWin.value <- tclVar(stepWin)
  make.common.EPS.value <- tclVar(make.common.EPS)
  make.select.period.EPS.value <- tclVar(make.select.period.EPS)
  first.year.common.value <- tclVar(first.year.common)
  last.year.common.value <- tclVar(last.year.common)

  frame4.label <-
    tklabel(frame4, text = " - EPS analysis - ", foreground = "blue")

  run.win.analysis.cbut <-
    tkcheckbutton(frame4.1.1, text = "EPS window analysis", variable = run.win.analysis.value)
  tkpack(frame4.label, fill = "x")
  tkpack(run.win.analysis.cbut, fill = "x", side = "left")
  tkpack(frame4.1.1, fill = "x")

  winLength.entry <-
    tkentry(frame4.1.2, textvariable = winLength.value, width = 3)
  winLength.lab <- tklabel(frame4.1.2, text = "Window length: ")
  tkpack(winLength.lab, winLength.entry, side = "left")
  tkpack(frame4.1.2, fill = "x")

  stepWin.entry <-
    tkentry(frame4.1.3, textvariable = stepWin.value, width = 3)
  stepWin.lab <- tklabel(frame4.1.3, text = "                  Lag: ")
  tkpack(stepWin.lab, stepWin.entry, side = "left")
  tkpack(frame4.1.3, fill = "x")

  make.common.EPS.cbut <-
    tkcheckbutton(frame4.2.1, text = "Common interval", variable = make.common.EPS.value)

  tkpack(make.common.EPS.cbut, fill = "x", side = "left")


  make.select.period.EPS.cbut <-
    tkcheckbutton(frame4.2.2, text = "Period        First year:", variable = make.select.period.EPS.value)
  first.year.common.entry <-
    tkentry(frame4.2.2, textvariable = first.year.common.value, width = 4)
  tkpack(
    make.select.period.EPS.cbut,
    first.year.common.entry,
    fill = "x",
    side = "left"
  )

  last.year.common.entry <-
    tkentry(frame4.2.3, textvariable = last.year.common.value, width = 4)
  last.year.common.label <-
    tklabel(frame4.2.3, text = "                         Last year:")

  tkpack(last.year.common.label, last.year.common.entry, side = "left")

  tkpack(frame4.2.1,
    frame4.2.2,
    frame4.2.3,
    fill = "x",
    side = "top"
  )

  tkpack(frame4.1, frame4.2, fill = "x", side = "left")
  tkpack(frame4, fill = "x")

  frame.exit <- tkframe(tf, relief = "groove")



  OnOk <- function(...) {
    .Tcl(paste("wm state ", tf, "withdrawn"))
    tclvalue(done) <- 2
    input <- tclvalue(filenamevar)
    output <- tclvalue(tabnamevar)
    .assign("winLength", winLength <-
      .tcl2num(winLength.value))
    .assign(
      "run.win.analysis",
      run.win.analysis <- .tcl2num(run.win.analysis.value)
    )
    .assign("stepWin", stepWin <-
      pmax(1, toNumber(tclvalue(stepWin.value))))
    .assign(
      "make.common.EPS",
      make.common.EPS <- .tcl2num(make.common.EPS.value)
    )
    .assign(
      "make.select.period.EPS",
      make.select.period.EPS <- .tcl2num(make.select.period.EPS.value)
    )


    first.year.common <- .tcl2num(first.year.common.value)
    last.year.common <- .tcl2num(last.year.common.value)

    .assign("first.year.common", first.year.common)
    .assign("last.year.common", last.year.common)
    flag <- try(exists(tclvalue(filenamevar)), silent = TRUE)

    if (flag == TRUE) {
      stc <- .get("stc")
      rwl <- get(input)
      cat(paste("\nEPS analysis [", input, "]\n", sep = ""))
      if (run.win.analysis) {
        cat(rep("=", 94), sep = "")
        cat(paste("\nRunning analysis:\n", sep = ""), sep = "")

        output_win <- Run.Win(
          rwl,
          winLength = .get("winLength"),
          stc = .get("stc"),
          step = .get("stepWin")
        )
        for (i in c(0.75, 0.80, 0.85, 0.90)) {
          EPS.resume(output_win, EPS = i)
        }
      }

      if (as.logic(tclvalue(make.common.EPS.value))) {
        rwl_common <- common.interval(
          rwl = rwl,
          type = "years",
          make.plot = FALSE
        )
        EPS.common.interval(rwl_common, stc = stc, out = FALSE)
      }

      if (as.logic(tclvalue(make.select.period.EPS.value))) {
        if (!(is.null(first.year.common) ||
          is.null(last.year.common.value))) {
          EPS.common.interval(
            rwl,
            first.year.common = first.year.common,
            last.year.common = last.year.common,
            stc = stc,
            out = FALSE
          )
        }
      }
    }
  }


  fr.exit.space <- tklabel(frame.exit, text = " ")
  ok.but <-
    tkbutton(frame.exit, text = "      Ok      ", command = OnOk)
  cancel.but <-
    tkbutton(
      frame.exit,
      text = "    Cancel    ",
      command = function() {
        tkdestroy(tf)
      }
    )
  tkgrid(cancel.but, fr.exit.space, ok.but)
  tkpack(frame.exit, side = "right")
  tkbind(tf, "<Destroy>", function() {
    tclvalue(done) <- 2
  })
  tkbind(tf, "<KeyPress-Escape>", function() {
    tclvalue(done) <- 2
  })
  tkbind(tf, "<KeyPress-Return>", OnOk)
  tkbind(tf, "<KP_Enter>", OnOk)

  tkwait.variable(done)
  tkgrab.release(tf)
  if (tclvalue(done) == "2") {
    return(tkdestroy(tf))
  }
  # tkfocus(tt)
  # return(invisible(TRUE))
}

# interactiveEPS ()
