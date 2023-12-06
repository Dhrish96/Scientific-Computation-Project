# Load the Shiny library
library(shiny)

# Load the book_dataset_cleanv.csv file
local_csv <- read.csv("C:/Users/dhris/OneDrive/Desktop/my_app/book_dataset_cleanv.csv")

# Define the user interface layout
ui <- fluidPage(
  tags$head(         # Define the head section of the HTML
    tags$style(      # Define the style section within the head
      HTML(          # Use HTML to define CSS styles
        "
        body {
          background-image: url('https://www.wallpapertip.com/wmimgs/9-97960_26102-title-room-of-knowledge-man-made-book.jpg'); 
          background-size: cover;
          background-repeat: no-repeat;
          background-attachment: fixed;
          font-family: 'Georgia', Lato;
          color: #333;
        }
        /* CSS styles for different elements in the app */
        .title {
          text-align: center;
          font-size: 36px;
          margin-bottom: 20px;
        }
        /* Sidebar styles */
        .sidebar {
          background-color: rgba(255, 255, 255, 0.8);
          padding: 20px;
          border-radius: 10px;
          max-width: 600px; 
          margin: 0 auto; 
          
        }
        /* Main panel styles */
        .main-panel {
          background-color: rgba(255, 255, 255, 0.8);
          padding: 20px;
          border-radius: 10px;
        }
        /* Keyword input styles */
        #keywordInput {
          width: 100%;
          padding: 8px;
          margin-bottom: 10px;
          border-radius: 5px;
          border: 1px solid #ccc;
        }
        /* Submit button styles */
        #submit {
          width: 100%;
          padding: 8px;
          border-radius: 5px;
          background-color: #A0522D;
          color: white;
          border: none;
        }
        #submit:hover {
          background-color: #8B4513;
          cursor: pointer;
        }
        /* Table styles */
        table {
          width: 100%;
          border-collapse: collapse;
          margin-top: 20px;
        }
        th, td {
          padding: 8px;
          text-align: left;
          border-bottom: 1px solid #ddd;
        }
        "
      )
    )
  ),
  # Display the title panel with text 
  titlePanel("Book Recommender"),
  sidebarLayout(                     # Define a layout with sidebar and main panel
    sidebarPanel(                    # Define the sidebar panel
      div(class = "sidebar",         # Create a div with the class "sidebar"
          textInput("keywordInput", "Enter keyword:"),   # Input field to enter a keyword
          actionButton("submit", "Get Recommendations")  # Button to trigger book recommendations
      )
    ),
    # Define the main panel
    mainPanel(
      div(class = "main-panel",      # Create div with the class "main-panel"
          tableOutput("bookList")    # Display the output table for book recommendations
      )
    )
  )
)
# Define the server logic
server <- function(input, output) {
  filtered_books <- reactive({      # Create a reactive expression to filter books based on keyword input
    keyword <- input$keywordInput   # Get the keyword entered by the user
    
    # Checking if the keyword is not empty
    if (nchar(keyword) > 0) {
      keyword <- tolower(keyword)   # Convert the keyword to lowercase
      
      # search for books that contain the specific keyword and store in the 'matching_books' variable
      matching_books <- local_csv[grep(keyword, tolower(local_csv$contents)), ]
      
      # Convert empty strings in 'Author' column to NA
      matching_books$Author[matching_books$Author == ""] <- NA
      
      # Remove rows with NA or empty values in specified columns
      matching_books <- matching_books[complete.cases(matching_books$Title, matching_books$Author, matching_books$ISBN, matching_books$Rating), ]
      
      # Checking if there are matching books after filtering
      if (nrow(matching_books) > 0) {
        # Sort by 'Rating' column to get top-rated books
        top_books <- matching_books[order(matching_books$Rating, decreasing = TRUE), ]
        
        # Return the top 10 books
        return(head(top_books, 10))
      } else {
        # Return a message if no matching books found
        return(data.frame(message = "No matching books found for the entered keyword."))
      }
    } else {
      return(NULL)     # Return NULL if keyword is empty
    }
  })
  
  output$bookList <- renderTable({         # Render the table output for book recommendations
    req(input$submit)                      # Ensure that the submit button is pressed
    filtered_data <- req(filtered_books()) # Get the filtered data from the function 'filtered_books'
    
    if (is.null(filtered_data)) {
      return(NULL)                         # Return NULL if filtered data is NULL
    } else if ("message" %in% colnames(filtered_data)) {
      return(data.frame(message = filtered_data$message))  # Return a data frame with a message if no books found
    } else {
      filtered_books_subset <- filtered_data[, c("ISBN","Title", "Author", "Rating")] # Creating a subset of the filtered data containing specific columns ISBN, Title, Author, Rating
      return(filtered_books_subset)       # Return the subset of filtered data
    }
  })
}

# Run the Shiny application
shinyApp(ui, server)
