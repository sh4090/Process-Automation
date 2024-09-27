# End-to-End System/Process Automation: UK Companie House from API to Exploitable Google Sheet
This project came about during an internship at an Economic Intelligence firm. I was tasked with 
mapping out the corporate affiliations of a certain couple and they ended up having over 50 
affiliated companies in the UK alone, past and present. 

Retrieving the available information into a coherent and usable database, without accounting for 
the analysis, took me 2 full workdays, more or less. As I was going about this task, I couldn’t 
help but feel like I was doing robotic work. 

All I could think was: “I bet I could automate this”...
And I did.

## Project Overview
This project is an end-to-end system designed to automate the process of retrieving, cleaning, and 
analysing company information from multiple data sources, such as public APIs and internal databases,
then produce a comprehensive output in the form a Google Sheets file. It utilizes the UK Companies 
House REST API to fetch relevant data on companies, including corporate officers, significant 
controllers, financial filings, and charges. The system is highly scalable, capable of handling 
multiple companies at once, and it supports error handling for unmatched companies or variations 
in naming conventions.

I’ve compiled the core functions used in this project in an open-source R package you can download 
from this link LINK.

## Applications
This project has a number of applications, depending on the analysis of the output, which can be 
applied to due diligence and compliance, investment and financial research, as well as business 
intelligence and competitive analysis. By automating the extraction of critical corporate data—such as 
officer details, financial charges, and insolvency history—it enhances the efficiency and accuracy of 
assessing corporate financial health, brand risk or conducting competitive analysis. It all ties down 
to the way the data extracted is then leveraged.

## Key Features
##### Open-Source R Package: 
I created a set of functions encapsulated in a new open-source R package to perform the entire data 
collection and cleaning workflow (LINK).

Process Automation: Once set up, the system automates data extraction and analysis, requiring minimal human 
intervention, only the input of company names, allowing for near real-time updates when companies file new 
documents.

For my case, I tried to recreate the work I had done for my internship and the code extracted that same 
information in under 50 seconds, a 99.9% reduction in the time it took me to do it manually.

Data Cleansing & Standardization: The system automatically standardizes company names, deals with variations 
(e.g., Ltd, LLC) to ensure high match accuracy, and cleans retrieved data for easily analysable output later 
on.

REST API Wrangling: The system efficiently interacts with external data sources through REST APIs, ensuring 
smooth retrieval and integration of data.

Text Processing: The project uses advanced text processing techniques to analyze and categorize unstructured 
data

Google Sheets Integration: All retrieved data is logged into pre-structured Google Sheets, enabling easy 
review and collaboration across teams.

## If I had to develop this project further, I would … 
Add more conditions to the extraction of certain information. It seems that data from older companies is labeled 
differently when extracted using the API.

Explore extracting information from the pdf filings directly to provide a more detailed perspective on the 
company’s financial status, aside from charges and insolvency only.

Accommodate for companies that generate multiple matches, or for typos/special characters/non-Latin alphabets.

Explore potential for dynamic updates at changes in company information.
