# ICE Detention Data Analysis Project

**Note:** You can access and run the notebook yourself on Kaggle: [Messy ICE Detention Notebook](https://www.kaggle.com/code/aayushadhikari112/messy-ice-detention-notebook)

## Project Overview & Personal Journey

When I first encountered this messy ICE detention dataset, I was both excited and daunted. The data represented real-world information about detention facilities across the United States, and I knew that proper analysis could reveal important insights. However, the inconsistencies and errors in the data presented significant challenges that would test my data cleaning and analytical skills.

## Data Processing Journey: From Messy to Meaningful

### Initial Assessment & Challenges

Opening the raw `messy_ice_detention.csv` file was an eye-opening experience. I immediately noticed several issues:

- **Inconsistent Facility Names**: Many names contained random special characters (^, &, %), typos, and formatting inconsistencies
- **Problematic Location Data**: Missing cities, inconsistent state formats
- **Date Formatting Issues**: Inspection dates were stored as Excel numeric date serials instead of proper date values
- **Data Type Problems**: Population figures were in scientific notation and had inconsistent decimal formatting
- **Hidden Patterns**: It wasn't immediately clear how security levels A-D differed or what they represented

The first hour was honestly overwhelming. I had to fight the urge to rush in with quick fixes and instead develop a systematic approach to data cleaning.

### Step-by-Step Solution Process

#### 1. Data Loading & Exploration

I began by loading the data into R using the tidyverse package. My first step was understanding what I was working with:

```r
# Initial data import with proper handling of header rows
raw_data <- read_csv(
  file_path,
  skip = 7,  # Skip metadata rows
  col_names = c("Name", "City", "State", "Level_A", "Level_B", "Level_C", "Level_D", "Last_Inspection_Date"),
  na = c("NA", "N/A", "", " "),  # Define NA strings
  trim_ws = TRUE  # Trim whitespace
)
```

The revelation that the first 7 rows contained metadata instead of actual data was my first "aha" moment. Reading the file with `skip = 7` was essential to properly align the columns. Note that I've explicitly defined the column headings in the `col_names` parameter rather than using the 7th row as headers, as the 7th row contained the actual column names but I wanted to ensure consistency and proper naming convention.

#### 2. Name Cleaning & Standardization

The facility names were my biggest headache. I spent hours developing regex patterns to address various issues:

```r
# Cleaning facility names
cleaned_data <- raw_data %>%
  mutate(
    # Remove special characters from facility names
    Name = str_replace_all(Name, "[^A-Za-z0-9 \\-,\\.\\(\\)]", ""),
    
    # Fix common abbreviations and standardize format
    Name = case_when(
      str_detect(Name, "FDC") ~ str_replace(Name, "FDC", "FEDERAL DETENTION CENTER"),
      str_detect(Name, "CTR$|CNTR$") ~ str_replace(Name, "CTR$|CNTR$", "CENTER"),
      str_detect(Name, "CORR") & !str_detect(Name, "CORRECTION") ~ str_replace(Name, "CORR", "CORRECTIONAL"),
      TRUE ~ Name
    ),
    
    # Trim extra whitespace
    Name = str_squish(Name)
  )
```

Every time I thought I had a pattern covered, another exception would appear. I discovered names like "ALEXA$NDRIA", "ALL%EN PARISH", and "B^AKER COUNTY" that required careful cleaning.

#### 3. Location Data Standardization

Cleaning the city and state data was particularly satisfying. I created a validation function that checked each state code against official two-letter abbreviations:

```r
# Create reference data for state validation
state_codes <- c(setNames(state.abb, state.name), "DC" = "District of Columbia", 
                 "PR" = "Puerto Rico", "GU" = "Guam", "MP" = "Northern Mariana Islands")

cleaned_data <- cleaned_data %>%
  mutate(
    # Clean City names
    City = str_replace_all(City, "[^A-Za-z0-9 \\-]", ""),
    City = str_squish(City),
    
    # Standardize common city spellings
    City = case_when(
      City == "FTLAUDERDALE" ~ "FORT LAUDERDALE",
      City == "ST LOUIS" ~ "SAINT LOUIS",
      # More standardizations...
      TRUE ~ City
    ),
    
    # Clean State codes
    State = str_replace_all(State, "[^A-Za-z]", ""),  # Keep only letters
    State = str_to_upper(State),  # Ensure uppercase
    
    # Flag potentially incorrect states
    State_Valid = State %in% c(state.abb, "DC", "PR", "GU", "MP", "VI")
  )
```

Finding that several facilities used incorrect state codes made me realize how important this validation step was.

#### 4. Numeric Data Transformation

Converting the population levels from scientific notation to numeric values required careful handling of data types:

```r
# Clean and convert population columns
cleaned_data <- cleaned_data %>%
  mutate(
    # Convert to numeric, handling scientific notation appropriately
    across(
      .cols = starts_with("Level_"),
      .fns = ~ as.numeric(.)
    ),
    
    # Replace NAs with 0 for population counts
    across(
      .cols = starts_with("Level_"),
      .fns = ~ replace_na(., 0)
    ),
    
    # Round to nearest hundredth for consistency
    across(
      .cols = starts_with("Level_"),
      .fns = ~ round(., 2)
    )
  )
```

I was shocked to discover that some values were in scientific notation (e.g., "1.80E-02" instead of 0.018), which required special handling.

#### 5. Date Parsing & Validation

The inspection dates were stored as 5-digit numbers that initially made no sense to me. After research and experimentation, I realized they were Excel-style date serials:

```r
# Create a robust function to parse mixed date formats
parse_mixed_dates <- function(date_input) {
  # Try parsing as Excel numeric date (origin "1899-12-30" handles Excel's leap year bug)
  if (str_detect(date_input, "^\\d{5}$")) { 
    parsed_date <- suppressWarnings(as.Date(as.numeric(date_input), origin = "1899-12-30"))
    if (!is.na(parsed_date)) {
      # Validate the date is reasonable (after 2000, before 2030)
      if (year(parsed_date) >= 2000 && year(parsed_date) <= 2030) {
        return(parsed_date)
      }
    }
  }
  
  # Try parsing known date formats in order of likelihood
  date_formats <- c(
    "m/d/Y",      # 9/19/2024
    "m-d-Y",      # 9-19-2024
    "Y-m-d",      # 2024-09-19
    "d-m-Y",      # 19-09-2024
    "B d, Y"      # September 19, 2024
  )
  
  for (format in date_formats) {
    parsed_date <- suppressWarnings(parse_date_time(date_input, orders = format))
    if (!is.na(parsed_date)) {
      return(as.Date(parsed_date))
    }
  }
  
  # Return NA if all parsing attempts fail
  return(as.Date(NA))
}
```

Discovering this pattern was a genuine "eureka" moment that unlocked important temporal information in the dataset. The robust date parser I created could handle both Excel date serials and various text-based date formats.

#### 6. Derived Metrics & Classification

Creating the facility size classification helped bring meaning to the raw numbers:

```r
# Calculate total population and add categorization
analyzed_data <- cleaned_data %>%
  mutate(
    Total_Population = Level_A + Level_B + Level_C + Level_D,
    
    # Categorize facilities by size
    Facility_Size = case_when(
      Total_Population >= 1000 ~ "Large (1000+)",
      Total_Population >= 500 ~ "Medium (500-999)",
      Total_Population >= 100 ~ "Small (100-499)",
      TRUE ~ "Very Small (<100)"
    ),
    
    # Convert to factor with ordered levels
    Facility_Size = factor(Facility_Size, levels = c(
      "Very Small (<100)", "Small (100-499)", "Medium (500-999)", "Large (1000+)"
    ))
  )
```

This transformation turned abstract numbers into meaningful categories that helped me understand the facility landscape.

### Visualization Highlights & Discoveries

Creating effective visualizations was harder than I anticipated, but the results were enlightening:

#### Top 10 Facilities Visualization

The bar chart of the top 10 facilities revealed concentration patterns I hadn't expected:

```r
# Create an enhanced bar chart of the top 10 facilities
top_10_plot <- ggplot(top_10_facilities, aes(x = Total_Population, y = reorder(Name, Total_Population))) +
  geom_col(aes(fill = Total_Population)) +  # Color gradient by size
  geom_text(
    aes(label = comma(Total_Population, accuracy = 1)),  # Format numbers with commas
    hjust = -0.1,  # Position labels to the right of bars
    size = 3.5,    # Text size
    color = "black" # Ensure text is readable
  ) + 
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Use viridis color palette for accessibility
  labs(
    title = "Top 10 Largest ICE Detention Facilities by Total Population",
    subtitle = "Based on combined population across security levels A, B, C, and D",
    x = "Total Population",
    y = NULL  # No y-axis label needed
  ) +
  theme_minimal()
```

The visualization showed that the largest facilities were significantly larger than the next tier, with some facilities housing more than twice as many detainees as others in the top 10.

#### Security Level Distribution

My analysis of security levels revealed an important pattern:

```r
# Create a stacked bar chart showing the breakdown by security level
stacked_plot <- ggplot(top_10_stacked, 
                      aes(x = reorder(Name, -Population, sum), y = Population, fill = Security_Level)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Security Level Distribution in Top 10 ICE Detention Facilities",
    subtitle = "Breakdown of population across Levels A, B, C, and D",
    x = NULL,
    y = "Population",
    fill = "Security Level"
  ) +
  theme_minimal()
```

The visualization revealed that most detainees were housed in Level A security, with significant variations in how facilities distributed their populations across security levels.

#### Geographic Analysis 

The state-level analysis provided crucial geographic context:

```r
# Calculate total detention population by state
state_population <- analyzed_data %>%
  filter(!is.na(State) & State != "") %>%  # Remove rows with missing states
  group_by(State) %>%
  summarise(
    Facility_Count = n(),
    Total_Population = sum(Total_Population),
    Avg_Population = mean(Total_Population),
    Max_Population = max(Total_Population)
  ) %>%
  arrange(desc(Total_Population))

# Create a bar chart of the top 10 states
top_states_plot <- ggplot(head(state_population, 10), 
                         aes(x = Total_Population, y = reorder(State, Total_Population))) +
  geom_col(aes(fill = Facility_Count)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Top 10 States by ICE Detention Population",
    subtitle = "Color indicates number of facilities in each state",
    x = "Total Population",
    y = "State",
    fill = "Facility Count"
  )
```

The analysis revealed that Texas, California, and Louisiana had both the highest total detention populations and the most facilities, highlighting geographic concentration patterns in the detention system.

## Key Findings & Insights

Analysis of the cleaned dataset (`cleaned_ice_detention_data.csv`) revealed the following verified insights:

1. **Facility Size Distribution**: The majority of facilities (55.97%) are classified as "Very Small (<100)" detainees, while only 8.21% are classified as "Large (1000+)". Small facilities (100-499 detainees) comprise 19.40% and Medium facilities (500-999) account for 16.42%.

2. **Security Level Patterns**: Level A (highest security) contains 60.61% of all detainees, followed by Level C (14.83%), Level B (13.29%), and Level D (11.27%).

3. **Geographic Concentration**: The top three states (TX, CA, LA) account for 55.19% of the total detention population, with Texas alone housing 30.19% of all detainees, followed by Louisiana (17.38%) and California (7.63%).

4. **Inspection Timing**: The median time since last inspection was 185 days, with significant variation across facilities (standard deviation of 188 days). The most recent inspection was 45 days prior, while the oldest was 1,486 days (over 4 years).

5. **Data Quality Issues**: 1.49% of facility records had data quality issues that required correction or research.

## Verification Analysis
All claims presented in this analysis are derived from the cleaned dataset.

### Detailed Findings

1. **Facility Size Distribution**: 
   - The majority category is "Very Small (<100)" at 55.97% of all facilities
   - Only 8.21% of facilities are classified as "Large (1000+)"
   - Medium facilities (500-999) account for 16.42%
   - Small facilities (100-499) account for 19.40%

2. **Security Level Patterns**:
   - Level A houses the clear majority of detainees at 60.61%
   - Level C contains 14.83% of detainees
   - Level B contains 13.29% of detainees
   - Level D contains 11.27% of detainees

3. **Geographic Concentration**:
   - Top states by detention population: TX (30.19%), LA (17.38%), CA (7.63%), GA (6.31%), AZ (5.71%)
   - Top states by number of facilities: TX (23), LA (9), FL (9), CA (6), PA (5)

4. **Inspection Timing**:
   - Median days since last inspection: 185 days
   - Mean days since last inspection: 215 days
   - Range: 45 days to 1,486 days
   - Standard Deviation: 188 days

5. **Data Quality Issues**:
   - 1.49% of facilities had identified data quality issues

### Visualization Results
The analysis generated visualizations (stored in `verification_results/`):
- Facility size distribution chart showing the predominance of Very Small facilities
- Security level distribution showing the Level A majority
- Geographic concentration charts confirming Texas, Louisiana, and California as the top states

## Project Structure

```
.
├── messy_ice_detention.csv              # Original raw data
├── messy-ice-detention-notebook         # R analysis notebook with full code
└── results/                             # Output directory
    ├── cleaned_ice_detention_data.csv   # Processed and cleaned dataset
    ├── Rplot001.png                     # Initial exploratory plot
    ├── security_level_distribution.png  # Security level analysis
    ├── top_10_facilities_visualization.png  # Largest facilities
    └── top_states_by_population.png     # Geographic distribution
```


## Resources Used

### R Packages
- **[tidyverse](https://www.tidyverse.org/)**: Collection of R packages for data science, including dplyr, ggplot2, and readr
- **[lubridate](https://lubridate.tidyverse.org/)**: For handling dates and times in R
- **[stringr](https://stringr.tidyverse.org/reference/str_replace.html)**: For string manipulation, particularly regular expressions
- **[scales](https://scales.r-lib.org/)**: For formatting axes and legends in visualizations
- **[janitor](https://github.com/sfirke/janitor)**: For data cleaning and tabulation

### External Resources
- **Excel Date Conversion**: Resources for understanding Excel's date system:
  - [Stack Overflow: Convert Excel date serial number to regular date](https://stackoverflow.com/questions/13850605/convert-excel-date-serial-number-to-regular-date)
  - [Microsoft Support: Date systems in Excel](https://support.microsoft.com/en-gb/office/date-systems-in-excel-e7fe7167-48a9-4b96-bb53-5612a800b487)
  - [ChatGPT explanation](https://chatgpt.com/share/680d2f2f-d440-8011-89ff-1690e9f3abbe)

### Data Visualization References
- **[ggplot2 Reference](https://ggplot2.tidyverse.org/)**: For creating all visualizations
- **[Data Visualization Cheatsheet](https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)**: Quick reference for ggplot2 syntax

## Challenges & Lessons Learned

Throughout this project, I faced numerous challenges that pushed my data science skills:

1. **Data Format Inconsistency**: The unpredictable nature of the facility names required flexible cleaning approaches rather than rigid rules. I learned to build cleaning functions that could handle exceptions.

2. **Excel Date System**: Converting Excel-style dates (which count days since 1899-12-30, with a leap year bug) was particularly challenging and required research outside my existing knowledge base.

3. **Scientific Notation Handling**: Some population values used scientific notation, which initially caused visualization issues until I properly converted them.

4. **Outlier Identification**: Distinguishing between true outliers and data entry errors required developing robust z-score methods and manual verification.

5. **Comprehensive Date Parsing**: Creating a function that could handle multiple date formats (Excel serial numbers, m/d/Y, Y-m-d, etc.) was more complex than anticipated.

## Future Improvements

If I were to continue this project, I would:

1. Implement more advanced geospatial visualizations using coordinates and mapping libraries
2. Add time-series analysis to track changes in detention populations
3. Create an interactive dashboard for exploring the data (using Shiny)
4. Integrate additional datasets (like inspection results or facility types) for richer context
5. Improve the data validation to handle more edge cases
6. Add facility-level demographic analysis if such data became available

## Personal Reflection

This project taught me that real-world data is messy, challenging, and full of surprises. What initially seemed like a straightforward data cleaning task evolved into a deep exploration of data validation, transformation, and visualization techniques.

The most satisfying aspect was seeing how each cleaning step gradually transformed a chaotic dataset into structured, meaningful information. Though frustrating at times, the process of uncovering patterns in the noise reinforced why I'm passionate about data analysis.

## License

This project is licensed under the terms included in the LICENSE file.

---
*Note: This analysis was conducted for educational purposes and aims to provide objective insights into publicly available data.*