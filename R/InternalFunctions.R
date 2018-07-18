
#' Execute function
#'
#' Takes the location of a FUSION tool's executable, a list of its inputs and a list of its formal arguments.
#' All formals that are NOT listed as inputs are considered to be switches.
#' 'Switches and inputs are then formatted, and sent to the command line along with the executable file location.

.executeFunction <- function(toolName, inputs, formals, env = parent.frame()){

  # Remove inputs from list of parameters
  switches <- formals[!formals %in% c(inputs)]

  # Create function name
  execFile <- paste0(options()$rFUSION.path,  "\\", gsub("rFUSION::", "", toolName), ".exe")

  # Test to see whether or not file exists
  if(!file.exists(execFile)){
    if(!file.exists(options()$rFUSION.path)){
      stop("Could not find FUSION folder. Define FUSION folder using the \'FUSIONfolder()\' function.")
    }else{
      stop("Could not find ", paste0(toolName, ".exe."),
           " Define FUSION folder using the \'FUSIONfolder()\' function and check that FUSION is properly installed.")
    }
  }

  # Execute function
  system(paste(execFile,
               paste(.formatSwitches(switches, env), collapse = " "),
               paste(.formatInputs(inputs, env), collapse = " ")),
               show.output.on.console = options()$rFUSION.printToConsole)

  # Clean-up
  tempList <- paste(tempdir(), "\\", "laslist.txt", sep = "")
  if(file.exists(tempList)){file.remove(tempList)}
}


#' Format switches
#'
#' Take a list of switches for a FUSION tool and formats them accordingly.

.formatSwitches <- function(switches, env){

  # Cycle through input switches
  outCommands <- lapply(switches, function(switch){

    # Extract value of switch
    switchValue <- eval(parse(text=switch), envir = env)

    if(!is.null(switchValue)){

      if(switch == "gridxy" && GridXYAdjust()) switchValue <- .gridXYadjust(switchValue, env)

      if(any(is.na(switchValue))){

        # If the switch contains any NA values, then return NULL
        return(NULL)

      }else{
        # If the switch is anything other than NULL, numeric, character, or logical, then return an error
        if(!class(switchValue) %in% c("numeric", "character", "logical", "integer")){stop(paste("Invalid input for switch",switch))}

        # If the switch is a character, concatenate it, add quotation marks and return it
        if(is.character(switchValue)){

          ##########################################################
          # NOTE TO SELF:
          # This part is a bit funky. The only way to get the 'ground' switch to work with a file path argument
          # was to have the WHOLE switch under quotation marks, instead of the individual file path. The original
          # line:
          #
          # return(paste0("/",switch, ":", paste(paste0("\"", gsub("/", "\\\\", switchValue), "\""), collapse = ",")))}
          #
          # would have wrapped the individual path (or paths) and quotation marks. I'm not sure how the
          # current line would handle multiple file paths if those paths had any spaces in them
          ##########################################################

          return(paste0("\"/",switch, ":", paste(gsub("/", "\\\\", switchValue), collapse = ","), "\""))}


        # If the switch is logical...
        if(is.logical(switchValue)){
          if(switchValue){

            # If the switch is set to TRUE, then return it as a switch
            return(paste0("/",switch))
          }else{

            # If the switch is set to FALSE, then return NULL
            return(NULL)
          }}

        # If the switch is numeric, concatenate and return it
        if(is.numeric(switchValue)){
          return(paste0("/",switch, ":", paste(switchValue, collapse = ",")))}
      }
    }
  })

  # Remove switches with NULL values
  outCommands <- unlist(outCommands[!sapply(outCommands, is.null)])
  return(outCommands)
}

#' Format inputs
#'
#' Take alist of inputs for a FUSION tool and formats them accordingly.

.formatInputs <- function(inputs, env){

  # Cycle through inputs
  outCommands <- lapply(inputs, function(input){

    # Extract value of input
    inputValue <- eval(parse(text=input), envir = env)

    if(!is.null(inputValue)){
      if(any(is.na(inputValue))){

        # If the input contains any NA values, then return NULL
        stop(NULL)

      }else{
        # If the input is neither numeric or character then return an error
        if(!class(inputValue) %in% c("numeric", "character")){stop(paste("Invalid input for input",input))}

        # If the input is a character...
        if(is.character(inputValue)){

          # Remove forward slashes
          inputValue <- gsub("/", "\\\\", inputValue)

          # If the input has multiple file paths....
          if(all(grepl("\\.", inputValue)) & (length(inputValue) > 1)){

            # Create list as a temporary text file
            tempList <- paste(tempdir(), "\\", "laslist.txt", sep = "")
            write(inputValue, file = tempList)
            return(paste0("\"", tempList, "\""))

            # Otherwise, add quotations and concatenate
          }else{
            return(paste(paste0("\"", inputValue, "\""), collapse = " "))}}

        # If the input is numeric, return it as-is
        if(is.numeric(inputValue)){
          return(inputValue)}
      }
    }
  })

  # Remove inputs with NULL values
  outCommands <- unlist(outCommands[!sapply(outCommands, is.null)])
  return(outCommands)
}

#' Adjust Grid XY values

.gridXYadjust <- function(input, env){

  # Retrieve cell size from parent environment
  cellSize <- eval(parse(text='cellsize'), envir = env)

  # Format input according to it's class
  if(class(input) == "Extent"){

    input <- c(input@xmin, input@ymin, input@xmax, input@ymax)

  }else if(inherits(input, "Raster")){

    input <- extent(input)
    input <- c(input@xmin, input@ymin, input@xmax, input@ymax)

  }else if(class(input) == "character"){

    input <- try(extent(raster::raster(input)), silent = TRUE)
    if(is(input, "try-error")) stop("Invalid raster path for 'gridxy'")
    input <- c(input@xmin, input@ymin, input@xmax, input@ymax)

  }else if(class(input) == "numeric"){

    if(input[1] >= input[3] | input[2] >= input[4]) stop("Invalid coordinates for 'gridxy'")

  }else stop("Invalid input for 'gridxy'")

  # Name input elements
  names(input) <- c("xmin", "ymin", "xmax", "ymax")

  # Adjust for the fact that FUSION puts cell centres on the extent limits as opposed to the cell edges
  input["xmin"] <- input["xmin"] + (cellSize / 2)
  input["xmax"] <- input["xmax"] - (cellSize / 2)
  input["ymin"] <- input["ymin"] + (cellSize / 2)
  input["ymax"] <- input["ymax"] - (cellSize / 2)

  # Get range of X and Y axes
  x.range <- input["xmax"] - input["xmin"]
  y.range <- input["ymax"] - input["ymin"]

  # Round the range according to cell size
  x.range.round <- APfun::AProunder(x.range, cellSize, "down")
  y.range.round <- APfun::AProunder(y.range, cellSize, "down")

  # Readjust "ymin" and "xmax" accordingly
  input["ymin"] <- input["ymax"] - y.range.round
  input["xmax"] <- input["xmin"] + x.range.round

  return(input)
}
