ifnull <- function(arg) {
  if(is.null(arg)) return("")
  else return(arg)
}


#' @title Get JavaScript code for filters' selectizeInput onchange event
#'
#' @description Helper function to create the JavaScript event handler code for
#'   the \code{selectizeInput} filters of a shiny app using \code{shinyfilters}.
#'
#' @param name Name of the event/input variable set by the \code{selectizeInput}
#'   filters whenever the selection changes. Can be handled in a call of
#'   \code{observeEvent()}.
#'
#' @details Processing the \code{onChange} event of the \code{selectizeInput}
#'   widgets that serve as the filters is necessary so that filters all the
#'   other \code{shinyfilter} filters bound to the same \code{reactable} can be
#'   updated accordingly and show the currently available filter options. All
#'   \code{selectizeInput} should have exactly the same event handler.
#'
#'   You can of course create the JavaScript code for the \code{onChange} event
#'   handler function yourself, especially if you want to trigger additional
#'   operations in JavaScript whenever an \code{onChange} event occurs.
#'   \code{event()} function is just intended as a shortcut to save time and
#'   effort.
#'
#'   For a full example of a shiny app using \code{shinyfilter} please call up
#'   the help for \code{\link{update_filters}()}. See the \code{README.md} file
#'   or the GitHub repo on
#'   \href{https://github.com/jsugarelli/shinyfilter}{https://github.com/jsugarelli/shinyfilter}
#'   for a comprehensive \code{shinyfilter} tutorial.
#'
#' @return JavaScript code for the \code{onChange} event.
#'
#' @examples
#' event("myEvent")
#'
#' @export
event <- function(name) {
  return(I(paste0("function(value) { Shiny.setInputValue('", name, "', Math.random()); }")))
}



#' @title Define the set of interdependent filters
#'
#' @description Installs the filters and binds them to the \code{reactable}
#'   widget and the dataframe presented in the \code{reactable}.
#'
#'   \code{define_filters()} needs to be called in the server function of any
#'   shiny app using \code{shinyfilter}.
#'
#' @param input The input object provided as an argument to the server function.
#' @param react_id Object ID/input slot of the \code{reactable} which the
#'   filters will be linked to.
#' @param filters A named character vector with the column names of the
#'   dataframe that will be filtered. The \emph{names} of the vector elements
#'   are the object IDs/input slots of the respective \code{selectizeInput()}
#'   widgets used as filters.
#' @param data The (unfiltered) dataframe presented in the \code{reactable}.
#'
#'
#' @details For a full example of a shiny app using \code{shinyfilter} please
#'   call up the help for \code{\link{update_filters}()}. See the
#'   \code{README.md} file or the GitHub repo on
#'   \href{https://github.com/jsugarelli/shinyfilter}{https://github.com/jsugarelli/shinyfilter}
#'   for a comprehensive \code{shinyfilter} tutorial.
#'
#'
#' @return No return value.
#' @export

define_filters <- function(input, react_id, filters, data) {
  cur_sel <- list()
  sf <- list()
  for(i in 1:NROW(filters)) {
    cur_sel[[i]] <- eval(paste0("input$", names(filters)[i]))
  }
  names(cur_sel) <- names(filters)

  if(exists("shinyfilters.r", envir=parent.env(environment()))) {
    sf <- shiny::isolate(shiny::reactiveValuesToList(get("shinyfilters.r", envir=parent.env(environment()))))
    if(react_id %in% names(sf$filters.internal)) nxt <- which(names(sf$filters.internal) == react_id)[1]
    else nxt <- length(sf$filters.internal) + 1
    filters.internal <- sf$filters.internal
    filters.internal[[nxt]] <- list(filters = filters, data = data, cur_sel = cur_sel)
    names(filters.internal)[nxt] <- react_id
  }
  else {
    lst <- list(filters = filters, data = data, cur_sel = cur_sel)
    filters.internal = list(lst)
    names(filters.internal)[1] <- react_id
  }
  sf.list <- shiny::reactiveValues(filters.internal = filters.internal)
  assign("shinyfilters.r", sf.list, envir= parent.env(environment()))
}




#' @title Update the filter options in each filter when the selection
#'   in any of the filters changes
#'
#' @description Updates all filters linked to a \code{reactable}. As
#'   \code{shinyfilter} filters are interdependent, \code{update_filters()}
#'   makes sure that each filter (\code{selectizeInput} widget) only shows the
#'   filter options currently available, given the selection in all other
#'   filters.
#'
#' @param input The input object provided as an argument to the server function.
#' @param session The session variable provided as an argument to the server
#'   function.
#' @param react_id The output variable/ID of the \code{reactable} for which
#'   filters will be updated.
#'
#' @return The filtered dataframe to be presented in the \code{reactable}
#'   widget. Ideally, this is captured in a reactive value so that the
#'   \code{reactable} updates automatically.
#'
#' @details See below for a full example of a shiny app using
#'   \code{shinyfilter}. See the \code{README.md} file or the GitHub repo on
#'   \href{https://github.com/jsugarelli/shinyfilter}{https://github.com/jsugarelli/shinyfilter}
#'    for a comprehensive \code{shinyfilter} tutorial.
#'
#'
#' @examples
#'
#' library(shiny)
#' library(reactable)
#' library(shinyfilter)
#'
#' cars_csv <- system.file("cars.csv", package="shinyfilter")
#'
#' cars <- read.csv(cars_csv, stringsAsFactors = FALSE, header = TRUE, encoding = "UTF-8")
#'
#' app = shinyApp(
#'   ui <- fluidPage(
#'     titlePanel("Cars Database"),
#'     sidebarLayout(
#'       sidebarPanel(
#'         width = 2,
#'
#'         selectizeInput(inputId = "sel_manufacturer", label = "Manufacturer",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$manufacturer))),
#'
#'         selectizeInput(inputId = "sel_year", label = "Year",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$year))),
#'
#'         selectizeInput(inputId = "sel_fuel", label = "Fuel",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$fuel))),
#'
#'         selectizeInput(inputId = "sel_condition", label = "Condition",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$condition))),
#'
#'         selectizeInput(inputId = "sel_size", label = "Size",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$size))),
#'
#'         selectizeInput(inputId = "sel_transmission", label = "Transmission",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$transmission))),
#'
#'         selectizeInput(inputId = "sel_color", label = "Color",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$paint_color))),
#'
#'         selectizeInput(inputId = "sel_type", label = "Type",
#'                        multiple = TRUE, options = list(onChange = event("ev_click")),
#'                        choices = sort(unique(cars$type))),
#'         use_tooltips(background = "#'  1B3F8C", foreground = "#'  FFFFFF")
#'       ),
#'       mainPanel(
#'         reactableOutput(outputId = "tbl_cars")
#'       )
#'     )
#'   ),
#'
#'
#'   server = function(input, output, session) {
#'
#'     r <- reactiveValues(mycars = cars)
#'
#'     define_filters(input,
#'                    "tbl_cars",
#'                    c(sel_manufacturer = "manufacturer",
#'                      sel_year = "year",
#'                      sel_fuel = "fuel",
#'                      sel_condition = "condition",
#'                      sel_size = "size",
#'                      sel_transmission = "transmission",
#'                      sel_color = "paint_color",
#'                      sel_type = "type"),
#'                    cars)
#'
#'
#'     observeEvent(input$ev_click, {
#'       r$mycars <- update_filters(input, session, "tbl_cars")
#'       update_tooltips("tbl_cars",
#'                       session,
#'                       tooltips = TRUE,
#'                       title_avail = "Available is:",
#'                       title_nonavail = "Currently not available is:",
#'                       popover_title = "My filters",
#'                       max.avail = 10,
#'                       max.nonavail = 10)
#'     })
#'
#'
#'     output$tbl_cars <- renderReactable({
#'       reactable(data = r$mycars,
#'                 filterable = TRUE,
#'                 rownames = FALSE,
#'                 selection = "multiple",
#'                 showPageSizeOptions = TRUE,
#'                 paginationType = "jump",
#'                 showSortable = TRUE,
#'                 highlight = TRUE,
#'                 resizable = TRUE,
#'                 rowStyle = list(cursor = "pointer"),
#'                 onClick = "select"
#'       )
#'     })
#'
#'   }
#'  )
#'
#'  \dontrun{
#'    runApp(app)
#'  }
#'
#' @export
update_filters <- function(input, session, react_id) {
  sf.list <- shiny::isolate(shiny::reactiveValuesToList(get("shinyfilters.r", envir=parent.env(environment()))))
  sf <- sf.list$filters.internal[[react_id]]
  data.new <- sf$data
  change <- c(rep(FALSE, NROW(sf$filters)))

  for(i in 1:NROW(sf$filters)) {
    col <- which(names(data.new) == sf$filters[i])[1]  # Filter column in the data
    sel <- eval(parse(text = paste0("input$", names(sf$filters))[i]))

    if(all(ifnull(sort(sel)) != ifnull(sort(sf$cur_sel[[i]])))) {
      change[i] <- TRUE
      sf$cur_sel[[i]] <- ifnull(sel)
    }
    if(!is.null(sel)) data.new <- data.new[data.new[,col] %in% sel,]
  }
  if(sum(change) > 0){
    for(i in 1:length(sf$filters)) {
      shiny::updateSelectizeInput(session, names(sf$filters)[i], choices = sort(unique(data.new[,sf$filters[i]])), selected = sf$cur_sel[i])
    }
  }

  avail <- list()
  non.avail <- list()
  for(i in 1:length(sf$filters)) {
    avail[[i]] <- sort(unique(data.new[,sf$filters[i]]))
    non.avail[[i]] <- sort(unique(sf$data[,sf$filters[i]][!(sf$data[,sf$filters[i]] %in% avail[[i]])]))
  }
  sf$avail <- avail
  sf$non.avail <- non.avail

  sf.list$filters.internal[[react_id]] <- sf
  sfx <- shiny::reactiveValues(filters.internal = sf.list$filters.internal)
  assign("shinyfilters.r", sfx, envir=parent.env(environment()))
  return(data.new)
}




#' @title Update the tooltips/popovers based on the currently available filter
#'   options
#'
#' @description Updates all tooltips or popovers for \code{shinyfilter} filter
#'   \code{selectizeInput} widgets. Tooltips/popovers can be used to show the
#'   currently unavailable filter options, i.e. the filter options that are not
#'   available at the moment because of the dataframe presented in the
#'   \code{reactable} is filtered by the choices made in the other filters. It
#'   is also possible to list the available filter options as well.
#'
#'   If you want to use tooltips/popovers, you need to call
#'   \code{\link{use_tooltips}()} from within the UI definition of your shiny
#'   app.
#'
#' @param react_id  The output variable/ID of the \code{reactable} to which the
#'   filters are linked.
#' @param session The session variable provided as an argument to the server
#'   function.
#' @param tooltip If \code{TRUE}, tooltips will be shown. If \code{FALSE},
#'   popovers will be shown.
#' @param show_avail If \code{TRUE} not only the unavailable filter options will
#'   be listed in the tooltips/popovers, but the unavailable ones as well.
#' @param title_avail Header text for the list of available filter options.
#' @param title_nonavail Header text for the list of unavailable filter options.
#' @param popover_title Title text for the popover window. Only relevant when
#'   \code{tooltips = FALSE}.
#' @param max_avail Maximum number of available filter options shown. Use the
#'   \code{more_avail} argument to determine what is shown if the number of
#'   available filter options exceeds \code{max_avail}.
#' @param max_nonavail Maximum number of non-available filter options shown. Use
#'   the \code{more_nonavail} argument to determine what is shown if the number
#'   of non-available filter options exceeds \code{max_nonavail}.
#' @param more_avail Text to be shown if \code{show_avail = TRUE} and the number
#'   of available filter options exceeds \code{max_avail}. In this case, only
#'   the first \code{max_avail} filter options are shown followed by
#'   \code{more_avail}. In \code{more_avail} you can use \code{#} as a
#'   placeholder for the number of filter options exceeding \code{max_avail}.
#' @param more_nonavail Text to be shown if the number of available filter
#'   options exceeds \code{max_nonavail}. In this case, only the first
#'   \code{max_nonavail} filter options are shown followed by
#'   \code{more_nonavail}. In \code{more_nonavail} you can use \code{#} as a
#'   placeholder for the number of filter options exceeding \code{max_nonavail}.
#' @param placement Defines where the tooltip/popover is placed relative to the
#'   filter (i.e. \code{selectizeInput} widget) it belongs to. Can be either
#'   \code{"top"}, \code{"bottom"}, \code{"left"} or \code{"right"}.
#'
#' @return No return value.
#'
#' @details For a full example of a shiny app using \code{shinyfilter} please
#'   call up the help for \code{\link{update_filters}()}. See the
#'   \code{README.md} file or the GitHub repo on
#'   \href{https://github.com/jsugarelli/shinyfilter}{https://github.com/jsugarelli/shinyfilter}
#'    for a comprehensive \code{shinyfilter} tutorial.
#'
#' @export
update_tooltips <- function(react_id, session, tooltip = TRUE, show_avail = TRUE, title_avail = "Available values:",
                            title_nonavail = "Currently not available filters:", popover_title = "Filter options",
                            max_avail = NULL, max_nonavail = max_avail, more_avail = "... (# more)",
                            more_nonavail = "... (# more)", placement = "top") {
  sf.list <- shiny::isolate(shiny::reactiveValuesToList(get("shinyfilters.r", envir=parent.env(environment()))))
  sf <- sf.list$filters.internal[[react_id]]
  avail <- sf$avail
  non.avail <- sf$non.avail


  for(i in 1:length(sf$filters)) {
    text <- ""

    if(!is.null(max_avail)) {
      n.avail <- NROW(avail[[i]])
      if(n.avail > max_avail) {
        avail[[i]] <- avail[[i]][1:max_avail+1]
        if(!is.null(more_avail)) {
          avail[[i]][max_avail+1] = stringr::str_replace(more_avail, "#", as.character(n.avail - max_avail))
        }
        else avail[[i]][max_avail+1] = "..."
      }
    }
    if(!is.null(max_nonavail)) {
      n.nonavail <- NROW(non.avail[[i]])
      if(n.nonavail > max_nonavail) {
        non.avail[[i]] <- non.avail[[i]][1:max_nonavail+1]
        if(!is.null(more_nonavail)) {
          non.avail[[i]][max_nonavail+1] = stringr::str_replace(more_nonavail, "#", as.character(n.nonavail - max_nonavail))
        }
        else non.avail[[i]][max_avail+1] = "..."
      }
    }

    if((!is.na(avail[[i]][1])) & show_avail) text <- paste0("<p><b>", title_avail, "</b></p>",
                                                            paste0(avail[[i]], collapse = "<br>"))
    if(show_avail & !is.na(avail[[i]][1])) text <- paste0(text, "<br><br>")
    if(!is.na(non.avail[[i]][1])) text <- paste0(text,"<p><b>", title_nonavail, "</b></p>",
                                                 paste0(non.avail[[i]], collapse = "<br>"))

    if(tooltip) shinyBS::addTooltip(session, names(sf$filters)[i], text, placement, trigger = "hover", options = list(html = TRUE))
    else shinyBS::addPopover(session, names(sf$filters)[i], popover_title, text, placement, trigger = "hover", options = list(html = TRUE))
  }
}




#' @title Add tooltip functionality to the app
#'
#' @description Prepares the application for the use of tooltips or popovers to
#'   show the (un)available filter options. \code{use_tooltips()} needs to be
#'   called from within the UI definition of your shiny app. See
#'   \code{\link{update_tooltips}()} for how to create the actual tooltips or
#'   popovers.
#'
#' @param background Background color of the tooltips/popovers the in CSS hex
#'   format.
#' @param foreground Font color of the tooltips/popovers the in CSS hex format.
#' @param textalign Alignment of the text in the tooltips/popovers; either
#'   \code{"left"}, \code{"right"}, \code{"center"} or \code{"justify"}.
#' @param fontsize Font size of the tooltips/popovers.
#' @param opacity Opacity of the tooltips/popovers.
#'
#' @return No return value.
#'
#' @details For a full example of a shiny app using \code{shinyfilter} please
#'   call up the help for \code{\link{update_filters}()}. See the
#'   \code{README.md} file or the GitHub repo on
#'   \href{https://github.com/jsugarelli/shinyfilter}{https://github.com/jsugarelli/shinyfilter}
#'    for a comprehensive \code{shinyfilter} tutorial.
#'
#' @export
use_tooltips <- function(background = "#000000", foreground = "#FFFFFF", textalign = "left",
                         fontsize = "100%", opacity = 0.8) {
  res <-list(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(
      list(
        ".tooltip > .tooltip-inner" = c(paste0("background-color: ", background),
                                        paste0("color: ", foreground),
                                        paste0("text-align: ", textalign),
                                        paste0("font-size: ", fontsize)),
        ".tooltip.in" = paste0("opacity: ", opacity, "!important"),
        ".tooltip.bottom > .tooltip-arrow" = paste0("border-bottom-color: ", background),
        ".tooltip.top > .tooltip-arrow" = paste0("border-top-color: ", background),
        ".tooltip.right > .tooltip-arrow" = paste0("border-right-color: ", background),
        ".tooltip.left > .tooltip-arrow" = paste0("border-left-color: ", background)
      )
    ),
    shinyBS::bsTooltip(paste0("X", as.integer(stats::runif(1)*10000000)), "", placement = "bottom", trigger = "hover")
  )
  return(res)
}
