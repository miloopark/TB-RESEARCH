# TB Management Policies in Prisons Around the World

## Two Different Analysis

- **Country Analysis:**  
  Pick a country from the dropdown, and the app instantly updates to show:
  1. **TB Incidence by WHO Region (Chart):** See how the country’s TB incidence rate compares within its region.
  2. **Barriers to TB Guidelines (Textual Details):** Read actual narrative details from the dataset about the country’s challenges—no complicated charts, just direct text from the data.
  3. **Time Trends in Prison TB Incidence (Line Chart):** If data is available, watch how TB incidence in prisons shifts over the years.
  4. **Raw Data Table:** Dive into the underlying dataset for that specific country to see all the details at once.

- **Global Analysis:**  
  Switch over to the Global Analysis tab and look at the bigger picture:
  1. **Global Comparison of Prison-Specific Guidelines (Chart):** How many countries have prison-specific guidelines? How many just mention prisons as high-risk? How many don’t mention them at all?
  2. **Frequency of Barriers (Chart):** Identify which barriers are most widespread across different countries.
  3. **Barrier Presence Heatmap:** A grid showing which countries face which barriers—perfect for spotting patterns and common challenges at a glance.

## Running the App

1. **Prerequisites:**
   - You need R and the **Shiny**, **ggplot2**, **dplyr**, **plotly**, **DT**, **bslib**, and **tidyr** packages installed.
   
2. **Launch the App:**
   ```r
   library(shiny)
   runApp("path_to_your_repository")
