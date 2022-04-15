arMODEL <- function(input = "", ...) {
  if (input == "<No active dataset>") {
    input <- ""
  }

  output <- ""
  if (input != "") {
    output <- paste(input, "res", sep = ".")
  }
  filenamevar <- tclVar(input)
  tabnamevar <- tclVar(output)
  arValue <- tclVar(.get("arMAX"))

  win <- tktoplevel()
  tkwm.geometry(win, paste("+0+", .get(".height"), sep = ""))

  tkwm.title(win, "AR model")
  tkwm.resizable(win, 0, 0)
  tkwm.deiconify(win)
  tkgrab.set(win)
  tkfocus(win)

  choose.data <- function() {
    input <- tk_select.list(sort(listDataSets()), title = "Select one")
    output <- paste(input, "res", sep = ".")
    tkgrab.set(win)
    if (input != "") {
      tkdelete(file.entry, 0, "end")
      tkinsert(file.entry, "end", input)
      tkdelete(tab.entry, 0, "end")
      tkinsert(tab.entry, "end", output)
    }
  }


  done <- tclVar(0)
  tfAR <- tkframe(win, relief = "groove")
  tkpack(tfAR, side = "top")
  frame1.a <- tkframe(tfAR, relief = "groove")
  frame1 <- tkframe(tfAR, relief = "groove")

  tkgrid(tklabel(frame1.a, text = "Options:", foreground = "blue"))
  tkpack(frame1.a, fill = "x")

  tab.entry <- tkentry(frame1, textvariable = tabnamevar)
  file.entry <- tkentry(frame1, textvariable = filenamevar)
  choosefile.but <-
    tkbutton(
      frame1,
      text = "...",
      command = function() {
        choose.data()
      }
    )
  tkgrid(
    tklabel(frame1, text = "Input name: "),
    file.entry,
    tklabel(frame1, text = " "),
    choosefile.but,
    sticky = "w"
  )
  tkgrid(tklabel(frame1, text = "Output name:"), tab.entry, sticky = "w")
  tkpack(frame1, fill = "x")

  frame3 <- tkframe(tfAR, relief = "groove")
  frame3.1 <- tkframe(frame3)

  makeAr.value <- tclVar(.get("makeAr"))
  arMAX.value <- tclVar(.get("arMAX"))
  arMAX.lab <-
    tklabel(frame3.1, text = "AR model of max order:", foreground = "blue")

  slider <- tkscale(
    frame3.1,
    from = 1,
    to = 10,
    showvalue = T,
    variable = arValue,
    resolution = 1,
    orient = "horizontal"
  )
  tkgrid(arMAX.lab, slider)
  tkpack(frame3.1, fill = "x")
  tkpack(frame3, fill = "x")

  frame.exit <- tkframe(tfAR, relief = "groove")

  OnOk <- function() {
    flag <- try(exists(tclvalue(filenamevar)), silent = TRUE)
    if (flag == TRUE) {
      # TODO
      eval(parse(
        text = paste(
          "ArFunction(",
          tclvalue(filenamevar),
          ", order.max=",
          as.numeric(tclvalue(arValue)),
          ")",
          sep = ""
        )
      ))
      # TODO
      eval(parse(
        text = paste(
          tclvalue(tabnamevar),
          "<<-apply(",
          tclvalue(filenamevar),
          ", 2,ar.func, order.max=",
          as.numeric(tclvalue(arValue)),
          ")",
          sep = ""
        )
      ))
      tclvalue(done) <- 2
    }
    .assign("arMAX", as.numeric(tclvalue(arValue)))
  }

  fr.exit.space <- tklabel(frame.exit, text = " ")
  ok.but <- tkbutton(frame.exit,
    text = "      Ok      ",
    command = OnOk
  )
  cancel.but <- tkbutton(
    frame.exit,
    text = "    Cancel    ",
    command = function() {
      tkdestroy(win)
    }
  )
  tkgrid(cancel.but, fr.exit.space, ok.but)
  tkpack(frame.exit, side = "right")

  tkbind(tfAR, "<Destroy>", function() {
    tclvalue(done) <- 2
  })
  tkbind(tfAR, "<KeyPress-Escape>", function() {
    tkdestroy(tfAR)
  })
  tkbind(tfAR, "<KeyPress-Return>", OnOk)
  tkbind(tfAR, "<KP_Enter>", OnOk)
  tkfocus(tfAR)
  tkwait.variable(done)
  if (tclvalue(done) == "2") {
    tkgrab.release(tfAR)
  }
  tkdestroy(win)
}
