library(shiny)
library(shinyMatrix)

### The GUI with a side panel containing the difficulty settings and the main panel where the puzzle should appear
ui <- fluidPage(
  
  titlePanel("R Sudoku"),
  
  sidebarLayout(
    sidebarPanel( #The sidebar containing our difficulty selection and "Generate puzzle" button
      width = 3, style="color: #000000; background-color: #ffe6ff; border-color: #cc0099",
      actionButton("showcase", "Showcase"),
      actionButton("easy", "Easy"),
      actionButton("normal", "Normal"),
      actionButton("hard", "Hard"),
      hr(style = "border-top: 1px solid #cc0099;"), # a nice divider
      textOutput("selectedDifficulty"),
      uiOutput("proceedButton"),
    ),
    
    mainPanel( #Main panel where the sudoku and new buttons will appear after "generate puzzle" is pressed
      width = 9,
      uiOutput("thePuzzle"), 
    )
  )
)



### Define server logic
server <- function(input, output, session) {
  
  ## MAKING THE SUDOKU
  
  # FIRST WE SELECT A FEW PREMADE SUDOKU MATRICES
  # (Calculating new ones from scratch would have to take a very long time, I have tried many methods and they're all simply too impractical)
  m1<-matrix(c(9,5,7,6,1,3,2,8,4,
               4,8,3,2,5,7,1,9,6,
               6,1,2,8,4,9,5,3,7,
               1,7,8,3,6,4,9,5,2,
               5,2,4,9,7,1,3,6,8,
               3,6,9,5,2,8,7,4,1,
               8,4,5,7,9,2,6,1,3,
               2,9,1,4,3,6,8,7,5,
               7,3,6,1,8,5,4,2,9),nrow=9, ncol=9)
  
  m2<-matrix(c(4,5,3,8,2,6,1,9,7,
               8,9,2,5,7,1,6,3,4,
               1,6,7,4,9,3,5,2,8,
               7,1,4,9,5,2,8,6,3,
               5,8,6,1,3,7,2,4,9,
               3,2,9,6,8,4,7,5,1,
               9,3,5,2,1,8,4,7,6,
               6,7,1,3,4,5,9,8,2,
               2,4,8,7,6,9,3,1,5),nrow=9, ncol=9)
  
  m3<-matrix(c(1,7,5,2,9,4,8,3,6,
               6,2,3,1,8,7,9,4,5,
               8,9,4,5,6,3,2,7,1,
               5,1,9,7,3,2,4,6,8,
               3,4,7,8,5,6,1,2,9,
               2,8,6,9,4,1,7,5,3,
               9,3,8,4,2,5,6,1,7,
               4,6,1,3,7,9,5,8,2,
               7,5,2,6,1,8,3,9,4),nrow=9, ncol=9)
  
  m4<-matrix(c(5,3,4,6,7,8,9,1,2,
               6,7,2,1,9,5,3,4,8,
               1,9,8,3,4,2,5,6,7,
               8,5,9,7,6,1,4,2,3,
               4,2,6,8,5,3,7,9,1,
               7,1,3,9,2,4,8,5,6,
               9,6,1,5,3,7,2,8,4,
               2,8,7,4,1,9,6,3,5,
               3,4,5,2,8,6,1,7,9),nrow=9, ncol=9)
  
  m5<-matrix(c(9,2,5,6,3,1,8,4,7,
               6,1,8,5,7,4,2,9,3,
               3,7,4,9,8,2,5,6,1,
               7,4,9,8,2,6,1,3,5,
               8,5,2,4,1,3,9,7,6,
               1,6,3,7,9,5,4,8,2,
               2,8,7,3,5,9,6,1,4,
               4,9,1,2,6,7,3,5,8,
               5,3,6,1,4,8,7,2,9),nrow=9, ncol=9)
  
  matrices<-list(m1,m2,m3,m4,m5) #We put the matrices into a list to be easily selected later
  
  # Next we will need to scramble the numbers within a matrix, while making sure it still works as a sudoku. This can be done by only scrambling rows/columns within sections of three.
  # I made a function that scrambles either rows or columns based on our input.
  ## FUNCTION FOR MIXING UP ONE SECTION OF THE PUZZLE
  swapping<-function(isRow) {
    numSwaps<- sample (5:20,1) # We choose a random number (from an arbitrarily chosen range) of the rows/columns to be swapped
    
    for (k in 1:numSwaps) {
      swapSection<-sample(c(1,4,7),1) # We randomly select (for each new "swapping") one of the thirds of rows/columns - 1 corresponds to the first, 4 to the second a nd 7 to the third - since these are the indexes of the first row/col within them 
      col1<- swapSection+sample(0:2,1) # We get two indexes for rows/cols from our selected section
      col2<- swapSection+sample(0:2,1)
      while (col1 == col2) {col2 <- swapSection+sample(0:2,1)} # We make it so they are not the same row/column
      
      if (isRow==T) { #We swap the given rows or cols based on the value input into our function
        swappedGrid[c(col1,col2), ] <<- swappedGrid[c(col2,col1), ]
      } else {
        swappedGrid[ ,c(col1,col2)] <<- swappedGrid[ ,c(col2,col1)]
      }
    }
    return(swappedGrid)
  }
  
  # FUNCTION FOR SELECTING ONE OF THE MATRIXES AND MIXING IT UP
  fullGrid<-function() { # This function selects one of the grid matrixes and mixes it up using the function we made above
    
    originalGrid<<-matrices[[sample(length(matrices), 1)]] # This is not really necessary but allows us to check which grid was selected (it helped me while troubleshooting)
    swappedGrid<<-originalGrid
    swapping(isRow=T) #First we swap the rows
    swapping(isRow=F) # Then the columns
    if (runif(1) <= 0.5) { # We make it so there is a 50% chance of transposing (flipping rows and columns)
      swappedGrid <<- t(swappedGrid)
    }
    
    return(swappedGrid)
  }
  
  # Now we have a full sudoku grid and we need to make it into a fill-able puzzle:
  # FUNCTION FOR REMOVING SOME OF THE NUMBERS
  removeStuff <- function(grid, removals) {
    for (k in 1:removals) { # We use a number of removals determined by selected difficulty
      while (TRUE) {
        i <- sample(1:9, 1)
        j <- sample(1:9, 1) # We get random coordinates to turn blank
        if (grid[i, j] != " ") { # We make sure we don't try to turn th same field blank more than one time (this would make the puzzle easier than intended)
          grid[i, j] <- " "
          break
        }
      }
    }
    return(grid)
  }
  
  # PUT IT ALL TOGETHER TO MAKE A PUZZLE
  # Now we combine all of the functions that we have made
  makePuzzle <- function() {
    grid <- fullGrid()
    Puzzle<<-removeStuff(grid=grid, removals=removals)
    return(Puzzle)
  }
  
  
  
  
  ## SOLUTION CHECKER
  checkSolution <- function(puzzleInput) {
    
    # We replace any blanks spots with 0s
    puzzleInput[puzzleInput==" "]<-0
    # We convert our solved matrix into a numerical one (before, when we replaced some cells with blanks we made it non-numerical)
    # when we do this, all non-numbers get turned into NA
    puzzle<-matrix(as.numeric(puzzleInput), ncol=9, nrow=9)
    
    # CHECKING:
    
    # We check if any numbers outside the range or if any non-numbers were input
    # This way we check for both NAs and numbers outside the range
    if(any(is.na(puzzle))|length(setdiff(puzzle, 1:9))>0){
      problem<<-paste("Invalid input value!")
      return(FALSE)
    }
    
    # We see if there were any 0s (blanks) left
    if(any(puzzle==0)){
      problem<<-paste("Please finish solving the puzzle!")
      return(FALSE)
    }
    
    # We check the rows for duplicates
    for (i in 1:nrow(puzzle)) {
      if (any(duplicated(puzzle[i, ]))) {
        problem<<-paste("Mistake in row", i, "!")
        return(FALSE)
      }
    }
    # We check the columns for duplicates
    for (i in 1:ncol(puzzle)) {
      if (any(duplicated(puzzle[, i]))) {
        problem<<-paste("Mistake in column", i, "!")
        return(FALSE)
      }
    }
    # We check the sections for duplicates
    for (i in seq(1, nrow(puzzle), by = 3)) {
      for (j in seq(1, ncol(puzzle), by = 3)) {
        if (any(duplicated(puzzle[i:min(i+2, nrow(puzzle)), j:min(j+2, ncol(puzzle))]))) {
          problem<<-paste("Mistake in section", i, "!")
          return(FALSE)
        }
      }
    }
    
    return(TRUE)
  }
  
  
  
  
  ## THE UI LOGIC:
  
  ### SIDE PANEL:
  # Setting the difficulty and also showing which one we have selected using buttons:
  removals<<-0 # First we set this value to zero
  
  observeEvent(input$showcase, {
    removals<<-2 #this decides how many empty fields will be in our sudoku
    output$selectedDifficulty <- renderText({
      paste("Selected difficulty: Showcase") # this shows our selected difficulty in text in the gui
    })
  })
  
  observeEvent(input$easy, {
    removals<<-20
    output$selectedDifficulty <- renderText({
      paste("Selected difficulty: Easy")
    })
  })
  
  observeEvent(input$normal, {
    removals<<-35
    output$selectedDifficulty <- renderText({
      paste("Selected difficulty: Normal")
    })
  })
  
  observeEvent(input$hard, {
    removals<<-55
    output$selectedDifficulty <- renderText({
      paste("Selected difficulty: Hard")
    })
  })
  
  # After setting our difficulty, the "Generate puzzle" button appears:
  observeEvent(any(input$showcase,input$normal,input$easy,input$hard), {
    output$proceedButton <- renderUI({
      req(any(input$showcase,input$normal,input$easy,input$hard,cancelOutput=FALSE))
      actionButton("generatePuzzle", "Generate new puzzle", style="color: #fff; background-color: #cc0099; border-color: #cc0099")
    })
  })
  
  
  
  ### MAIN PANEL:
  # Generating the sudoku and making it appear along all new parts of the UI:
  observeEvent(any(input$showcase,input$normal,input$easy,input$hard)&input$generatePuzzle, {
    
    output$result<- renderText({}) # these two lines reset things that are only supposed to show up if a correctly solved sudoku is submitted
    output$congrats<-renderUI({})  # if the user solves a puzzle and generates a new one, putting this here ensures the congratulatory message won't be there anymore
    
    output$thePuzzle <- renderUI({
      req(input$generatePuzzle,cancelOutput = FALSE) # This makes this part of UI hidden until "Generate puzzle" is pressed
      # for some reason, just putting it all under observeEvent doesn't work
      puzzle <- makePuzzle() #This is what gives the command to actually make a new sudoku puzzle 
      
      fluidRow(
        # How the actual sudoku will show up:
        column(width=7,
               matrixInput("puzzle", value = puzzle, rows = list(names = FALSE), cols = list(names = FALSE)),
        ),
        # The column with a reset and check solution button:
        column(width=5,
               actionButton("resetButton", "Reset puzzle",style="color: #000000; background-color: #ffe6ff; border-color: #cc0099"),
               actionButton("checkButton", "Check solution", style="color: #fff; background-color: #cc0099; border-color: #cc0099"),
               hr(style = "border-top: 1px solid #cc0099;"), # a nice divider
               uiOutput("result"), # Where the mistakes the user made or a positive message if solved correctly will appear
               uiOutput("congrats") # A nice image to show if user is successful
        )
      )
      
    })
  })
  
  # The button for resetting the generated puzzle:
  # It loads the same UI as generating a new puzzle does, except it uses the puzzle we already made
  # There might be a more elegant ways to do this but I wasn't able to make anything else work :(
  observeEvent(input$resetButton, {
    
    output$result<- renderText({}) #these two lines reset things that are only supposed to show up if a correctly solved sudoku is submitted
    output$congrats<-renderUI({}) 
    
    output$thePuzzle <- renderUI({
      puzzle<-Puzzle #This is the slightly different part - we use the already generated puzzle
      
      fluidRow(
        column(width=7,
               matrixInput("puzzle", value = puzzle, rows = list(names = FALSE), cols = list(names = FALSE)),
               
        ),
        column(width=5,
               actionButton("resetButton", "Reset puzzle", style="color: #000000; background-color: #ffe6ff; border-color: #cc0099"),
               actionButton("checkButton", "Check solution", style="color: #fff; background-color: #cc0099; border-color: #cc0099"),
               hr(style = "border-top: 1px solid #cc0099;"),
               uiOutput("result"),
               uiOutput("congrats")
        )
      )
      
    })
  })
  
  # The button for checking your solution:
  observeEvent(input$checkButton, {
    solution<-input$puzzle
    
    if(checkSolution(solution)==T){ # If the user's solution is correct, this message will appear:
      output$result<- renderText({
        paste("Congratulations! You have solved this puzzle!")
      })
      output$congrats<-renderUI({
        tags$img(src = "https://usagif.com/wp-content/uploads/2020/05/confetti.gif", width = 200, height = 200) 
      })
    }else{ # If it's not correct, it will tell the user what the problem is:
      output$result<- renderText({problem})
    }
  })
  
  
}

# Running the app
shinyApp(ui = ui, server = server)




