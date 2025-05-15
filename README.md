# Smart Feedback View for Training Feedback Dashboard

A dynamic and interactive R Shiny dashboard designed to visualize training participant feedback. This tool offers an intuitive way to explore participant responses, identify trends, and generate actionable insights from both quantitative and qualitative data.

## Features

- **Real-time Data Visualization**  
  Interactive charts that provide a clear overview of participant ratings across various service and content dimensions.

- **AI-Powered Summaries**  
  Uses Gemini API to summarize open-ended responses, helping users quickly understand key themes and sentiments.

- **Period and Training Filters**  
  Filter data by year, training title, and month to narrow down your analysis.

- **Secure Google Sheets Integration**  
  Automatically pulls feedback data from Google Sheets using service account authentication.

## Technologies Used

- **R**  
- `shiny`  
- `shinydashboard`  
- `googlesheets4`  
- `httr`, `jsonlite` (for API communication)  
- `ggplot2`, `dplyr`, `tidyr` (for data processing and visualization)

## Author
Muhammad Yoga Prabowo
