# Dynamic Shiny Application for Automated Retrieval and Structuring of UK Company Data via Companies House REST API
Website link: https://wqkuks-sara-hassani.shinyapps.io/Dynamic-CompaniesHouse-App/
Presentation link: 

This project came about during an internship at an Economic Intelligence firm. I was tasked with 
mapping out the corporate affiliations of a certain couple and they ended up having over 50 
affiliated companies in the UK alone, past and present. 

Retrieving the available information into a coherent and usable database, without accounting for 
the analysis, took me 2 full workdays, more or less. As I was going about this task, I couldn’t 
help but feel like I was doing robotic work. 

All I could think was: “I bet I could automate this”...
And I did.

## Project Overview
This project is a full-stack dynamic Shiny web app designed to automate the process of retrieving, cleaning, and 
analysing company information from multiple data sources, such as public APIs and internal databases,
then produce a comprehensive output in the form of a summary table in addition to dynamic data visualization tools. 
It utilizes the UK Companies House REST API to fetch relevant data on companies, including corporate officers, significant 
controllers, financial filings, and charges. The system is highly scalable, capable of handling 
multiple companies at once, and it supports error handling for unmatched companies or variations 
in naming conventions.

## Applications
This project has a number of applications, depending on the analysis of the output, which can be 
applied to due diligence and compliance, investment and financial research, as well as business 
intelligence and competitive analysis. By automating the extraction of critical corporate data—such as 
officer details, financial charges, and insolvency history—it enhances the efficiency and accuracy of 
assessing corporate financial health, brand risk or conducting competitive analysis. It all ties down 
to the way the data extracted is then leveraged.

## Key Features
##### Custom Functions: 
I developed a suite of purpose-built R functions to support the full data pipeline. These functions handle company name resolution, API querying, nested data extraction (e.g., officers, PSCs, charges), entity-level data cleaning, and output structuring. 

##### Process Automation: 
Once set up, the system automates data extraction and analysis, requiring minimal human intervention, 
only the input of company names, allowing for near real-time updates when companies file new documents.

For my case, I tried to recreate the work I had done for my internship and the code extracted that same 
information in under 50 seconds, a 99.9% reduction in the time it took me to do it manually.

##### Data Cleansing & Standardization: 
The system automatically standardizes company names, deals with variations (e.g., Ltd, LLC) to ensure high 
match accuracy, and cleans retrieved data for easily analysable output later on.

##### REST API Wrangling: 
The system efficiently interacts with external data sources through REST APIs, ensuring smooth retrieval 
and integration of data.

##### Text Processing: 
The project uses advanced text processing techniques to analyze and categorize semi-structured data.

##### Dynamic Data Handling
All retrieved data is logged into a pre-structured table labeled "Raw Data", enabling seamless review, export, and further manipulation. The Shiny app's interactive side panel includes custom filtering tools, allowing users to conduct targeted exploratory data analysis directly within the interface.

##### Dynamic Data Visualization
The application includes built-in visualization components—specifically, pie charts and bar plots—that are automatically generated based on both the full dataset and any applied filters. This enables users to instantly identify patterns, distributions, and anomalies without leaving the app environment.

## If I had to develop this project further, I would … 
Add more conditions to the extraction of certain information. It seems that data from older companies is labeled 
differently when extracted using the API.

Explore extracting information from the pdf filings directly to provide a more detailed perspective on the 
company’s financial status, aside from charges and insolvency only.

Accommodate for companies that generate multiple matches, or for typos/special characters/non-Latin alphabets.

Explore potential for dynamic updates at changes in company information.
