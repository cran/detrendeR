detrenderGUI <- function() {
  tt <- tktoplevel()
  tkwm.resizable(tt, 1, 0)
  done <- tclVar(0)
  tkwm.title(tt, "Define settings")
  tkwm.deiconify(tt)
  tkgrab.set(tt)

  # TREE MASK #####
  frame1.parent <- tkframe(tt, relief = "groove", borderwidth = 2)
  frame1 <-
    tkframe(frame1.parent, relief = "groove", borderwidth = 2)
  frame1.1 <- tkframe(frame1, relief = "groove", borderwidth = 0)
  frame1.2 <- tkframe(frame1, relief = "groove", borderwidth = 0)
  frame1.3 <- tkframe(frame1, relief = "groove", borderwidth = 0)
  tkpack(tklabel(frame1, text = "    - Tree Mask -    ", foreground = "blue"))

  stc <- .get("stc")
  site.value <- tclVar(stc[1])
  tree.value <- tclVar(stc[2])
  core.value <- tclVar(stc[3])

  site.entry <-
    tkentry(frame1.1, textvariable = site.value, width = 3)
  site.lab <- tklabel(frame1.1, text = "Site: ")
  tkpack(site.lab, site.entry, side = "left")

  tree.entry <-
    tkentry(frame1.2, textvariable = tree.value, width = 3)
  tree.lab <- tklabel(frame1.2, text = "Tree:")
  tkpack(tree.lab, tree.entry, side = "left")

  core.entry <-
    tkentry(frame1.3, textvariable = core.value, width = 3)
  core.lab <- tklabel(frame1.3, text = "Core:")
  tkpack(core.lab, core.entry, side = "left")

  tkpack(frame1.1, frame1.2, frame1.3, side = "top")
  tkpack(frame1, side = "left", fill = "both")
  tkpack(frame1.parent, fill = "both")

  # SELECT SERIES #####
  frame.1 <-
    tkframe(frame1.parent, relief = "groove", borderwidth = 2)

  remove.shorter.series.value <-
    tclVar(.get("remove.shorter.series"))
  delete.shorter.series.value <-
    tclVar(.get("delete.shorter.series"))

  tkgrid(tklabel(frame.1, text = "          - Select Series - ", foreground = "blue"))
  select.series.cbut <-
    tkcheckbutton(frame.1, text = "Delete series shorter than:", variable = remove.shorter.series.value)
  select.series.entry <-
    tkentry(frame.1, textvariable = delete.shorter.series.value, width = 4)

  tkgrid(select.series.cbut, select.series.entry)
  tkpack(frame.1, fill = "x")

  # MAKE SEGMENT PLOT ######
  frame.1.1 <-
    tkframe(frame1.parent, relief = "groove", borderwidth = 2)
  makeSegPlot.value <- tclVar(.get("makeSegPlot"))
  tkgrid(tklabel(frame.1.1, text = " - Plot Series - ", foreground = "blue"))

  makeSegPlot.cbut <-
    tkcheckbutton(frame.1.1, text = "Make graph with length series", variable = makeSegPlot.value)
  tkgrid(makeSegPlot.cbut)
  tkpack(frame.1.1, fill = "x")

  # DETRENDING #####
  frame2 <- tkframe(tt, relief = "groove", borderwidth = 2)
  tkpack(tklabel(frame2, text = " - Detrending - ", foreground = "blue"))

  interactive.detrend.value <- tclVar(.get("interactive.detrend"))
  interactive.detrend.cbut <-
    tkcheckbutton(frame2, text = "Interactive detrending", variable = interactive.detrend.value)
  tkpack(interactive.detrend.cbut,
    side = "bottom",
    anchor = "w"
  )

  Det1frame <-
    tkwidget(
      frame2,
      "labelframe",
      foreground = "blue",
      text = "First detrend: ",
      relief = "groove",
      borderwidth = 2
    )

  Det2frame <-
    tkwidget(
      frame2,
      "labelframe",
      foreground = "blue",
      text = "Second detrend: ",
      relief = "groove",
      borderwidth = 2
    )

  Det1.1frame <-
    tkframe(Det1frame, relief = "groove", borderwidth = 0)
  Det1.2frame <-
    tkwidget(
      Det1frame,
      "labelframe",
      foreground = "blue",
      text = "Spline options: ",
      relief = "groove",
      borderwidth = 2
    )

  Det1.2.1frame <-
    tkframe(Det1.2frame, relief = "groove", borderwidth = 0)
  Det1.2.2frame <-
    tkframe(Det1.2frame, relief = "groove", borderwidth = 0)
  Det1.2.3frame <-
    tkframe(Det1.2frame, relief = "groove", borderwidth = 0)

  Det2.1frame <-
    tkframe(Det2frame, relief = "groove", borderwidth = 0)

  Det2.2frame <-
    tkwidget(
      Det2frame,
      "labelframe",
      foreground = "blue",
      text = "Spline options: ",
      relief = "groove",
      borderwidth = 2
    )

  Det2.2.1frame <-
    tkframe(Det2.2frame, relief = "groove", borderwidth = 0)
  Det2.2.2frame <-
    tkframe(Det2.2frame, relief = "groove", borderwidth = 0)
  Det2.2.3frame <-
    tkframe(Det2.2frame, relief = "groove", borderwidth = 0)

  method1.value <- tclVar(.get("method1"))
  method2.value <- tclVar(.get("method2"))
  n1.value <- tclVar(.get("n1"))
  nPerc1.value <- tclVar(.get("nPerc1"))
  p1.value <- tclVar(.get("p1"))
  n2.value <- tclVar(.get("n2"))
  nPerc2.value <- tclVar(.get("nPerc2"))
  p2.value <- tclVar(.get("p2"))

  detrend.types <- c("Neg Exp", "Spline", "Spline%", "Mean", "No Detrending")
  detrend.values <- c("ModNegExp", "Spline", "Spline%", "Mean", "No Detrending")
  RadioButton(Det1.1frame,
    variable = method1.value,
    BUTTON = detrend.types,
    VALUE = detrend.values
  )
  RadioButton(Det2.1frame,
    variable = method2.value,
    BUTTON = detrend.types,
    VALUE = detrend.values
  )

  n1.entry <-
    tkentry(Det1.2.1frame, textvariable = n1.value, width = 5)
  Det1.2.1lab <- tklabel(Det1.2.1frame, text = "Spline length:")
  tkpack(Det1.2.1lab, n1.entry, side = "left")

  nPerc1.entry <-
    tkentry(Det1.2.2frame, textvariable = nPerc1.value, width = 5)
  Det1.2.2lab <- tklabel(Det1.2.2frame, text = "Spline ratio:  ")

  tkpack(Det1.2.2lab,
    nPerc1.entry,
    side = "left",
    anchor = "w"
  )

  p1.entry <-
    tkentry(Det1.2.3frame, textvariable = p1.value, width = 5)
  Det1.2.3lab <- tklabel(Det1.2.3frame, text = "Value of p:    ")
  tkpack(Det1.2.3lab, p1.entry, side = "left", anchor = "w")

  tkpack(Det1.2.1frame, Det1.2.2frame, Det1.2.3frame, side = "top")

  n2.entry <-
    tkentry(Det2.2.1frame, textvariable = n2.value, width = 5)
  Det2.2.1lab <- tklabel(Det2.2.1frame, text = "Spline length:")
  tkpack(Det2.2.1lab, n2.entry, side = "left")

  nPerc2.entry <-
    tkentry(Det2.2.2frame, textvariable = nPerc2.value, width = 5)
  Det2.2.2lab <- tklabel(Det2.2.2frame, text = "Spline ratio:  ")

  tkpack(Det2.2.2lab,
    nPerc2.entry,
    side = "left",
    anchor = "w"
  )

  p2.entry <-
    tkentry(Det2.2.3frame, textvariable = p2.value, width = 5)
  Det2.2.3lab <- tklabel(Det2.2.3frame, text = "Value of p:    ")
  tkpack(Det2.2.3lab, p2.entry, side = "left", anchor = "w")

  tkpack(Det2.2.1frame, Det2.2.2frame, Det2.2.3frame, side = "top")

  tkpack(
    Det1.1frame,
    Det1.2frame,
    side = "left",
    expand = 1,
    fill = "x"
  )

  tkpack(
    Det2.1frame,
    Det2.2frame,
    side = "left",
    expand = 1,
    fill = "x"
  )
  tkpack(
    Det1frame,
    Det2frame,
    side = "left",
    expand = 1,
    fill = "x"
  )

  tkpack(frame2, fill = "x")

  # AUTORREGRESIVE MODEL ###############################
  frame3 <- tkframe(tt, relief = "groove", borderwidth = 2)
  makeAr.value <- tclVar(.get("makeAr"))
  arMAX.value <- tclVar(.get("arMAX"))

  frame3.1 <-
    tkwidget(
      frame3,
      "labelframe",
      foreground = "blue",
      text = "Autorregresive model: ",
      relief = "groove",
      borderwidth = 2
    )
  makeAr.cbut <-
    tkcheckbutton(frame3.1, text = "Autorregresive model of order:", variable = makeAr.value)
  arMAX.entry <-
    tkentry(frame3.1, textvariable = arMAX.value, width = 2)

  tkgrid(makeAr.cbut, arMAX.entry)

  frame3.2 <-
    tkwidget(
      frame3,
      "labelframe",
      foreground = "blue",
      text = "Mean: ",
      relief = "groove",
      borderwidth = 2
    )

  rb1 <- tkradiobutton(frame3.2)
  rb2 <- tkradiobutton(frame3.2)
  rbValue <- tclVar(.get("biweightMean"))
  tkconfigure(rb1, variable = rbValue, value = TRUE)
  tkconfigure(rb2, variable = rbValue, value = FALSE)
  tkgrid(tklabel(frame3.2, text = "Robust     "), rb1)
  tkgrid(tklabel(frame3.2, text = "Arithmetic "), rb2)
  tkpack(
    frame3.1,
    frame3.2,
    side = "left",
    expand = 1,
    fill = "both"
  )

  tkpack(frame3, fill = "x")



  # EPS ################################

  frame4 <- tkframe(tt, relief = "groove", borderwidth = 2)
  frame4.1 <- tkframe(frame4, relief = "groove", borderwidth = 2)
  frame4.1.1 <-
    tkframe(frame4.1, relief = "groove", borderwidth = 0)
  frame4.1.2 <-
    tkframe(frame4.1, relief = "groove", borderwidth = 0)
  frame4.1.3 <-
    tkframe(frame4.1, relief = "groove", borderwidth = 0)

  frame4.2 <- tkframe(frame4, relief = "groove", borderwidth = 2)
  frame4.2.1 <-
    tkframe(frame4.2, relief = "groove", borderwidth = 0)
  frame4.2.2 <-
    tkframe(frame4.2, relief = "groove", borderwidth = 0)
  frame4.2.3 <-
    tkframe(frame4.2, relief = "groove", borderwidth = 0)

  run.win.analysis.value <- tclVar(.get("run.win.analysis"))
  winLength.value <- tclVar(.get("winLength"))
  stepWin.value <- tclVar(.get("stepWin"))
  make.common.EPS.value <- tclVar(.get("make.common.EPS"))
  make.select.period.EPS.value <- tclVar(.get("make.select.period.EPS"))
  first.year.common.value <- tclVar(.get("first.year.common"))
  last.year.common.value <- tclVar(.get("last.year.common"))

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
  stepWin.lab <-
    tklabel(frame4.1.3, text = "                  Lag: ")
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
  tkpack(frame4.1,
    fill = "both",
    expand = 1,
    side = "left"
  )
  tkpack(frame4.2,
    fill = "both",
    expand = 1,
    side = "right"
  )
  tkpack(frame4, fill = "x")


  OnOk <- function() {
    .assign("remove.shorter.series", as.logic(tclvalue(remove.shorter.series.value)))
    .assign(
      "delete.shorter.series",
      .tcl2num(delete.shorter.series.value)
    )
    .assign("makeSegPlot", as.logic(tclvalue(makeSegPlot.value)))
    .assign("interactive.detrend", as.logic(tclvalue(interactive.detrend.value)))
    .assign("makeFirstDetrending", TRUE)
    .assign("method1", method1 <- tclvalue(method1.value))
    .assign("n1", n1 <- .tcl2num(n1.value))
    .assign("nPerc1", nPerc1 <- toNumber(tclvalue(nPerc1.value)))
    .assign("p1", p1 <- toNumber(tclvalue(p1.value)))
    if (.get("method1") == "No Detrending") {
      .assign("makeFirstDetrending", FALSE)
    }
    .assign(
      "first_detrending_method",
      GetDetrendMethod(method1, n1, nPerc1, p1)
    )
    .assign("makeSecondDetrending", TRUE)
    method2 <- tclvalue(method2.value)
    .assign("method2", method2)
    .assign("n2", n2 <- .tcl2num(n2.value))
    .assign("nPerc2", nPerc2 <- toNumber(tclvalue(nPerc2.value)))
    .assign("p2", p2 <- toNumber(tclvalue(p2.value)))

    if (tclvalue(method2.value) == "No Detrending") {
      .assign("makeSecondDetrending", FALSE)
    }
    .assign(
      "second_detrending_method",
      GetDetrendMethod(method2, n2, nPerc2, p2)
    )
    .assign("makeAr", as.logic(tclvalue(makeAr.value)))
    .assign("arMAX", toNumber(tclvalue(arMAX.value)))
    .assign("biweightMean", as.logic(tclvalue(rbValue)))
    .assign("run.win.analysis", as.logic(tclvalue(run.win.analysis.value)))
    .assign("winLength", pmax(10, toNumber(tclvalue(
      winLength.value
    ))))
    .assign("stepWin", pmax(1, toNumber(tclvalue(stepWin.value))))
    .assign("make.common.EPS", as.logic(tclvalue(make.common.EPS.value)))
    .assign("make.select.period.EPS", as.logic(tclvalue(make.select.period.EPS.value)))
    .assign("first.year.common", toNumber(tclvalue(first.year.common.value)))
    .assign("last.year.common", toNumber(tclvalue(last.year.common.value)))

    if (.get("make.select.period.EPS")) {
      if (any(is.na(.get("first.year.common")), is.na(.get("last.year.common")))) {
        .assign("make.select.period.EPS", FALSE)
        .assign("make.common.EPS", TRUE)
      }
    }

    stc <-
      c(
        .tcl2num(site.value),
        .tcl2num(tree.value),
        .tcl2num(core.value)
      )
    if (sum(stc) != 8) {
      tk_messageBox(type = "ok", "Please correct the tree mask!")
    } else {
      .assign("stc", stc)
      tkdestroy(tt)
      tclvalue(done) <- 1
    }
  }


  frame5 <- tkframe(tt, relief = "groove", borderwidth = 2)
  Cancel.but <-
    tkbutton(
      frame5,
      text = "Cancel",
      command = function() {
        tkdestroy(tt)
      }
    )
  Ok.but <- tkbutton(frame5, text = "Ok", command = OnOk)
  tkpack(
    Cancel.but,
    Ok.but,
    side = "left",
    expand = "TRUE",
    fill = "x"
  )
  tkpack(frame5, fill = "x")

  tkfocus(tt)
  tkbind(tt, "<Destroy>", function() {
    tclvalue(done) <- 2
  })
  tkbind(tt, "<KeyPress-Escape>", function() {
    tkdestroy(tt)
  })
  tkbind(tt, "<KeyPress-Return>", OnOk)
  tkbind(tt, "<KP_Enter>", OnOk)

  tkwait.variable(done)
  tkdestroy(tt)
  if (tclvalue(done) == "2") {
    return(FALSE)
  }
  if (tclvalue(done) == "1") {
    return(TRUE)
  }
}
