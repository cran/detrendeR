add_radio_button <- function(frame,
                         variable = NULL,
                         button.name = c("b.r1", "b.r2"),
                         button.value = c(1, 2)) {
  button.name <- as.vector(button.name)
  for (i in 1:length(button.name)) {
    tkpack(
      tkradiobutton(
        frame,
        text = button.name[i],
        value = button.value[i],
        variable = variable
      ),
      anchor = "w"
    )
  }
}

is.empty <- function(x) {
  length(x) == 0
}

InteractiveDetrending <- function(rwl,
                                  detrend = NULL,
                                  method = "Mean",
                                  n = 32,
                                  nPerc = 0.67,
                                  p = 0.5,
                                  ...) {
  detrend.types <- c("Neg Exp", "Spline", "Spline%", "Mean")
  detrend.values <- c("ModNegExp", "Spline", "Spline%", "Mean")

  tk_current_series <- tclVar(0)
  tk_current_method <- tclVar(method)
  tk_current_n <- tclVar(n)
  tk_current_nPerc <- tclVar(nPerc)
  tk_current_p <- tclVar(p)
  tk_show_all_series <- tclVar(1)
  done <- tclVar(0)

  #inter_detrend_output <- detrend
  .assign("inter_detrend_output", inter_detrend_output <- detrend)
  .assign("inter_detrend_input", rwl)
  .seriesNames <- colnames(rwl)
  .x <- as.numeric(rownames(rwl))

  # detrending_method <- GetDetrendMethod(
  #   method = method,
  #   n = n,
  #   nPerc = nPerc,
  #   p = p
  # )
  inter_detrend_output_changes <-
    matrix(NA, ncol = 3, nrow = ncol(rwl))
  rownames(inter_detrend_output_changes) <- colnames(rwl)
  inter_detrend_output_changes[, 1] <- 1:ncol(rwl)
  inter_detrend_output_changes[, 2] <- colnames(rwl)
  inter_detrend_output_changes[, 3] <- GetDetrendMethod(
    method = method,
    n = n,
    nPerc = nPerc,
    p = p
  )

  .assign("inter_detrend_output_changes",
          inter_detrend_output_changes)

  if (is.null(detrend)) {
    inter_detrend_output <-
      try(apply(
        rwl,
        2,
        RemoveTrend,
        method = method,
        BandwidthPerc = nPerc,
        Bandwidth = n,
        P = p
      ),
      silent = TRUE)
    setVariable("inter_detrend_output",
                inter_detrend_output)
  }

  draw <- function(...) {
    op <- par(cex = 1.5)
    on.exit(par(op))
    rwl <- .get("inter_detrend_input")
    inter_detrend_output <-
      .get("inter_detrend_output")
    current_method <- .tcl2str(tk_current_method)
    current_spline_nPerc <- .tcl2num(tk_current_nPerc)
    current_spline_n <- .tcl2num(tk_current_n)
    current_spline_p <- .tcl2num(tk_current_p)
    seriesIndex <- .tcl2num(tk_current_series) + 1

    .assign("current_spline_nPerc", nPerc)
    .assign("current_spline_n", n)
    .assign("current_spline_p", p)

    show.all.series <-
      ifelse(.tcl2num(tk_show_all_series), "l", "n")

    matplot(
      .x,
      rwl,
      lty = 1,
      col = "grey",
      type = show.all.series,
      main = .seriesNames[seriesIndex],
      ylab = "",
      xlab = "Year",
      las = 1
    )
    temp <- rwl[, seriesIndex, drop = !FALSE]
    lines(.x, temp, type = "l", col = "red2")

    method_changed <-
      GetDetrendMethod(
        method = current_method,
        n = current_spline_n,
        nPerc = current_spline_nPerc,
        p = current_spline_p
      )
    mtext(
      paste(method_changed, sep = ""),
      line = 0.5,
      side = 3,
      adj = 1,
      cex = 1.25,
      col = "blue",
      font = 1
    )
    inter_detrend_output_changes <-
      .get("inter_detrend_output_changes")
    mtext(
      inter_detrend_output_changes[seriesIndex, 3],
      line = 0.5,
      side = 3,
      adj = 0,
      cex = 1.25,
      col = "black",
      font = 1
    )

    lines(.x, inter_detrend_output[, seriesIndex], col = "black")
    cv <- try(RemoveTrend(
      temp,
      method = current_method,
      BandwidthPerc = current_spline_nPerc,
      Bandwidth = current_spline_n,
      P = current_spline_p
    ),
    silent = TRUE)
    lines(.x, cv, col = "blue4")
  }

  #### win #####
  ttt <- tktoplevel()
  .assign("ttt", ttt)
  ttt <- tkRplot(ttt, function(...) {
    op <- par(cex = 1.5)
    on.exit(par(op))
    rwl <- .get("inter_detrend_input")
    inter_detrend_output <-
      .get("inter_detrend_output")
    current_method <- .tcl2str(tk_current_method)
    current_spline_nPerc <- .tcl2num(tk_current_nPerc)
    current_spline_n <- .tcl2num(tk_current_n)
    current_spline_p <- .tcl2num(tk_current_p)
    seriesIndex <- .tcl2num(tk_current_series) + 1

    .assign("current_spline_nPerc", nPerc)
    .assign("current_spline_n", n)
    .assign("current_spline_p", p)

    show.all.series <-
      ifelse(.tcl2num(tk_show_all_series), "l", "n")

    matplot(
      .x,
      rwl,
      lty = 1,
      col = "grey",
      type = show.all.series,
      main = .seriesNames[seriesIndex],
      ylab = "",
      xlab = "Year",
      las = 1
    )
    temp <- rwl[, seriesIndex, drop = !FALSE]
    lines(.x, temp, type = "l", col = "red2")

    method_changed <-
      GetDetrendMethod(
        method = current_method,
        n = current_spline_n,
        nPerc = current_spline_nPerc,
        p = current_spline_p
      )
    mtext(
      paste(method_changed, sep = ""),
      line = 0.5,
      side = 3,
      adj = 1,
      cex = 1.25,
      col = "blue",
      font = 1
    )
    inter_detrend_output_changes <-
      .get("inter_detrend_output_changes")
    mtext(
      inter_detrend_output_changes[seriesIndex, 3],
      line = 0.5,
      side = 3,
      adj = 0,
      cex = 1.25,
      col = "black",
      font = 1
    )

    lines(.x, inter_detrend_output[, seriesIndex], col = "black")
    cv <- try(RemoveTrend(
      temp,
      method = current_method,
      BandwidthPerc = current_spline_nPerc,
      Bandwidth = current_spline_n,
      P = current_spline_p
    ),
    silent = TRUE)
    lines(.x, cv, col = "blue4")
  })

  .tclTraceAddVariableWrite(get("tk_current_series"), arg = ttt)
  .tclTraceAddVariableWrite(get("tk_current_method"), arg = ttt)
  .tclTraceAddVariableWrite(get("tk_current_n"), arg = ttt)
  .tclTraceAddVariableWrite(get("tk_current_nPerc"), arg = ttt)
  .tclTraceAddVariableWrite(get("tk_current_nPerc"), arg = ttt)
  .tclTraceAddVariableWrite(get("tk_current_p"), arg = ttt)
  .tclTraceAddVariableWrite(get("tk_show_all_series"), arg = ttt)

  tkwm.title(ttt, "Interactive detrending")
  tkwm.deiconify(ttt)
  tkgrab.set(ttt)

  frame2 <- tkframe(ttt, relief = "groove", borderwidth = 0)
  frame2.1 <-
    tkwidget(
      frame2,
      "labelframe",
      text = " Series: ",
      relief = "groove",
      borderwidth = 2
    )
  frame2.2 <-
    tkwidget(
      frame2,
      "labelframe",
      text = " detrend method: ",
      relief = "groove",
      borderwidth = 2
    )
  frame2.3 <-
    tkwidget(
      frame2,
      "labelframe",
      text = " Spline options: ",
      relief = "groove",
      borderwidth = 2
    )
  frame2.4 <-
    tkwidget(
      frame2,
      "labelframe",
      text = "",
      relief = "groove",
      borderwidth = 2
    )
  frame2.4.1 <- tkframe(frame2.4)

  #### listbox ####
  scroll_bar <- tkscrollbar(
    frame2.1,
    repeatinterval = 25,
    command = function(...) {
      tkyview(list_box, ...)
    }
  )
  list_box <- tklistbox(
      frame2.1,
      height = 10,
      width = 20,
      selectmode = "single",
      yscrollcommand = function(...) {
        tkset(scroll_bar, ...)
      },
      background = "white"
    )
  tkgrid(list_box, scroll_bar)
  tkgrid.configure(scroll_bar, rowspan = 10, sticky = "nsw")

  flag <- try(series <- colnames(rwl), silent = TRUE)
  if (class(flag) != "try-error") {
    .assign("series", colnames(rwl))
    for (i in (1:length(series))) {
      tkinsert(list_box, "end", series[i])
    }
  }
  # tkselection.set(list_box,0)

  tkbind(list_box, "<ButtonRelease-1>", function(...) {
    tclvalue(tk_current_series) <- tkcurselection(list_box)
  })


  tkbind(list_box, "<Up>", function(...) {
    i <- as.numeric(tkcurselection(list_box))
    if (is.empty(i)) {
      i <- .tcl2num(tk_current_series)
    }
    if (i == 0) {
      i <- 1
    }
    tkinsert(list_box, i + 1, .seriesNames[i + 1])
    tkdelete(list_box, i)
    tclvalue(tk_current_series) <- i - 1
    tkselection.set(list_box, i - 1)
  })


  tkbind(list_box, "<Down>", function(...) {
    i <- as.numeric(tkcurselection(list_box))
    if (is.empty(i)) {
      i <- .tcl2num(tk_current_series)
    }
    tkselection.clear(list_box, i)
    if (i + 1 == length(.seriesNames)) {
      i <- i - 1
    }
    tclvalue(tk_current_series) <- i + 1
    tkselection.set(list_box, i + 1)
  })

  tkbind(list_box, "<KeyRelease>", function(...){
    i <- as.numeric(tkcurselection(list_box))
    if (is.empty(i)) {
      i <- .tcl2num(tk_current_series)
    }
    tclvalue(tk_current_series) <- i
    #tkselection.set(list_box, i) #do not work with Pg Up nor Pg Dn
  })

  ##### method ####
  add_radio_button(
    frame2.2,
    variable = tk_current_method,
    button.name = detrend.types,
    button.value = detrend.values
  )

  ###### scale_n ####
  scale_n <-
    tkscale(
      frame2.3,
      label = "Bandwidth (years)",
      from = 5,
      to = 1000,
      variable = tk_current_n,
      showvalue = TRUE,
      resolution = 5,
      orient = "hor"
    )
  tkpack(scale_n, fill = "x")

  ###### scale_nPerc ####
  scale_nPerc <-
    tkscale(
      frame2.3,
      label = "Bandwidth (ratio)",
      from = 0,
      to = 2,
      variable = tk_current_nPerc,
      showvalue = TRUE,
      resolution = 0.05,
      orient = "hor"
    )
  tkpack(scale_nPerc, fill = "x")

  ###### scale_p ####
  scale_p <-
    tkscale(
      frame2.3,
      label = "P",
      from = 0,
      to = 1,
      variable = tk_current_p,
      showvalue = TRUE,
      resolution = 0.05,
      orient = "hor"
    )

  tkpack(scale_p, fill = "x")

  tkpack(
    frame2,
    side = "left",
    expand = FALSE,
    # anchor = "c",
    before = ttt$env$canvas,
    fill = "both"
  )

  #### show all ####
  bt_show_all_series <- tkcheckbutton(frame2.4.1)
  tkconfigure(bt_show_all_series, variable = tk_show_all_series, text = "Show all series")
  tkpack(bt_show_all_series, side = "left", fill = "x")
  tkpack(frame2.4.1, fill = "both")

  tkpack(frame2.1, frame2.2, frame2.3, frame2.4, fill = "x")
  tkpack(frame2, fill = "y")


  update_curve <- function(...) {
    current_method <- .tcl2str(tk_current_method)
    current_spline_nPerc <- .tcl2num(tk_current_nPerc)
    current_spline_n <- .tcl2num(tk_current_n)
    current_spline_p <- .tcl2num(tk_current_p)
    seriesIndex <- .tcl2num(tk_current_series) + 1

    rwl <- .get("inter_detrend_input")
    inter_detrend_output <-
      .get("inter_detrend_output")
    inter_detrend_output_changes <-
      .get("inter_detrend_output_changes")
    inter_detrend_output_changes[seriesIndex, 3] <-
      GetDetrendMethod(
        method = current_method,
        n = current_spline_n,
        nPerc = current_spline_nPerc,
        p = current_spline_p
      )
    temp <- rwl[, seriesIndex]
    cv <-
      try(RemoveTrend(
        temp,
        method = current_method,
        BandwidthPerc = current_spline_nPerc,
        Bandwidth = current_spline_n,
        P = current_spline_p
      ),
      silent = TRUE)
    inter_detrend_output_changes[seriesIndex, 3] <-
      GetDetrendMethod(
        method = current_method,
        n = current_spline_n,
        nPerc = current_spline_nPerc,
        p = current_spline_p
      )
    inter_detrend_output[, seriesIndex] <- cv
    .assign("inter_detrend_output",
            inter_detrend_output)
    .assign("inter_detrend_output_changes",
            inter_detrend_output_changes)
    tclvalue(tk_current_method) <- current_method
  }


  close_tk_and_save <- function() {
    inter_detrend_output_changes <-
      .get("inter_detrend_output_changes")
    if (length(unique(inter_detrend_output_changes[, 3])) > 1) {
      inter_detrend_output_changes <-
        rbind(c("Seq", "Series    ", "Detrending            "),
              inter_detrend_output_changes)
      cat(rep("=", 38), "\n", sep = "")
      inter_detrend_output_changes[, 2] <-
        (format(inter_detrend_output_changes[, 2], justify = "left"))
      inter_detrend_output_changes[, 3] <-
        (format(inter_detrend_output_changes[, 3], justify = "left"))
      WriteMatrix(
        inter_detrend_output_changes,
        col.width = 3,
        ID = FALSE,
        row.names = F,
        col.names = F,
        sep = "|"
      )
      cat(rep("=", 38), "\n", sep = "")
    }
    tclvalue(done) <- 1
    .assign("inter_detrend_changed", TRUE)
  }

  close_tk_without_save <- function() {
    .assign("inter_detrend_changed", FALSE)
    tclvalue(done) <- 2
  }



  bt_update_curve <-
    (tkbutton(frame2.4, text = "Change", command = update_curve))
  bt_close <-
    (tkbutton(frame2.4, text = "Close without saving", command = close_tk_without_save))
  bt_save <-
    (tkbutton(frame2.4, text = "Save changes", command = close_tk_and_save))

  tkpack(bt_update_curve, fill = "both")
  tkpack(bt_close, fill = "both")
  tkpack(bt_save, fill = "both")

  #### binds ####

  tkbind(ttt, "<Destroy>", function() {
    tclvalue(done) <- 2
  })
  tkbind(ttt, "<KeyPress-Escape>", function() {
    tclvalue(done) <- 2
  })

  tkselection.set(list_box, 0)
  tkfocus(list_box)
  tkwait.variable(done)
  tkgrab.release(ttt)
  tkdestroy(ttt)
}
