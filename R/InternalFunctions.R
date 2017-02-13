
###########
### EXECUTE
###########

# This function takes the location of a FUSION tool's executable, a list of its inputs and a list of its formal arguments.
# All formals that are NOT listed as inputs are considered to be switches.
# Switches and inputs are then formatted, and sent to the command line along with the executable file location.

executeFunction <- function(toolName, inputs, formals, env = parent.frame()){

  # Remove inputs from list of parameters
  switches <- formals[!formals %in% c(inputs)]

  # Create function name
  execFile = paste0(options()$rFUSION.path,  "\\", toolName, ".exe")

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
               paste(formatSwitches(switches, env), collapse = " "),
               paste(formatInputs(inputs, env), collapse = " ")),
               show.output.on.console = options()$rFUSION.printToConsole)

  # Clean-up
  tempList <- paste(tempdir(), "\\", "laslist.txt", sep = "")
  if(file.exists(tempList)){file.remove(tempList)}
}

###################
### FORMAT SWITCHES
###################

# This function takes a list of switches for a FUSION tool and formats them accordingly.

formatSwitches <- function(switches, env){

  # Cycle through input switches
  outCommands <- lapply(switches, function(switch){

    # Extract value of switch
    switchValue <- eval(parse(text=switch), envir = env)

    if(!is.null(switchValue)){
      if(any(is.na(switchValue))){

        # If the switch contains any NA values, then return NULL
        return(NULL)

      }else{
        # If the switch is anything other than NULL, numeric, character, or logical, then return an error
        if(!class(switchValue) %in% c("numeric", "character", "logical")){stop(paste("ERROR: Invalid input for switch",switch))}

        # If the switch is a character, concatenate it, add quotation marks and return it
        if(is.character(switchValue)){
          return(paste0("/",switch, ":", paste(paste0("\"", switchValue, "\""), collapse = ",")))}

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

#################
### FORMAT INPUTS
#################

# This function takes a list of inputs for a FUSION tool and formats them accordingly.

formatInputs <- function(inputs, env){

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
