library(jsonlite)
library(httr)
library(googlesheets4)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

companies = "iceni group ltd, ech watches limited, gfu foods ltd, mansion mortgage services limited, valorem capital two ltd, skin-thetics limited, ventura sport limited, crisp finance limited, dvd-rent.co.uk limited, p2p networks limited, telepeople limited, mentor direct marketing limited, valorem capital one limited, phenolic limited, rozaya plc, tnc b realisations limited, thc realisations limited, thc h realisations limited, thc col realisations limited, fitness4less sc limited, thc c realisations limited, tnc bw realisations limited, thc nm realisations limited, thc m realisations limited, stamford brayham limited, hallburg limited, tf lb realisations limited, topnotch fitness (northampton) limited, western resources limited, crm recruitment limited, inter-alliance (group practices) limited, addiscombe group limited, psm international fasteners limited"

api_key = "a7d9c3b6-267d-4bf5-b070-2c780839d529"

sheet_url <- "https://docs.google.com/spreadsheets/d/1xDYmL5ch-nS3nxSOHbys-CNlL6IMpQde86ZU6JJ9FMQ/edit?gid=929024279#gid=929024279"
gs4_deauth()
sic = read_sheet(sheet_url) 

# SETTING UP FUNCTIONS

# Function to run search using the company name

company_name_search = function(api_key, company){
  
  # connect to API
  headers = c(
    api_key = api_key,
    'Authorization' = 'Basic YTdkOWMzYjYtMjY3ZC00YmY1LWIwNzAtMmM3ODA4MzlkNTI5Og=='
  )
  
  # standardize company name
  company_name = gsub("[^A-Za-z0-9 ]", "", tolower(company))
  company_name = gsub(" ", "+", company_name)
  
  # extract search pages
  url = paste0("https://api.company-information.service.gov.uk/search?q=", 
               company_name)
  res = VERB("GET", url = url, add_headers(headers))
  response_list = fromJSON(content(res, 'text'), flatten = TRUE)
  response_df = response_list$items
  
  # keep only companies
  response_df = subset(response_df, response_df$kind == "searchresults#company")
  
  # standardize company names from searches
  response_df$title_standard = gsub("[[:punct:]]", "", response_df$title)
  company = gsub("\\+", " ", toupper(company_name))
  
  index = which(response_df$title == company)
  name_number = data.frame(NAME = response_df$title[index], 
                           NUMBER = response_df$company_number[index], 
                           STATUS = response_df$company_status[index])
  ifelse(
    length(index) > 0, 
    return(name_number), 
    return("no match")
  )
}

# Function to obtain the company number from the company name

company_name_to_number = function(api_key, company_name){
  
  # standardizing the company name
  company = gsub("[^A-Za-z0-9 ]", "", tolower(company_name))
  # vector to store possible variations of the name
  options <- company
  
  # Check for the presence of abbreviations for company type 
  # to look for possible variation of the company name
  
  # Limited Liability Company
  if (grepl("\\bllc\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\bllc\\b", "limited liability company", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bllc\\b", "", company, ignore.case = TRUE))
  } else if (grepl("\\blimited liability company\\b$", 
                   company, ignore.case = TRUE)) 
  {
    options <- c(options, gsub("\\blimited liability company\\b", "llc", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\blimited liability company\\b", "", 
                               company, ignore.case = TRUE))
  }
  # Limited Partnership
  if (grepl("\\bltd\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\bltd\\b", "limited", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bltd\\b", "", company, ignore.case = TRUE))
  } else if (grepl("\\blimited\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\blimited\\b", "ltd", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\blimited\\b", "",
                               company, ignore.case = TRUE))
  }
  # Corporation
  if (grepl("\\bcorp\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\bcorp\\b", "corporation", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bcorp\\b", "", company, ignore.case = TRUE))
  } else if (grepl("\\bcorporation\\b$", company, ignore.case = TRUE)) 
  {
    options <- c(options, gsub("\\bcorporation\\b", "corp", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bcorporation\\b", "", 
                               company, ignore.case = TRUE))
  }
  # Public Limited Company
  if (grepl("\\bplc\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\bplc\\b", "public limited company", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bplc\\b", "", company, ignore.case = TRUE))
  } else if (grepl("\\bpublic limited company\\b$", 
                   company, ignore.case = TRUE)) 
  {
    options <- c(options, gsub("\\bpublic limited company\\b", "plc", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bpublic limited company\\b", "", 
                               company, ignore.case = TRUE))
  }
  # Limited Liability Partnership
  if (grepl("\\bllp\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\bllp\\b", "limited liability partnership", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bllp\\b", "", company, ignore.case = TRUE))
  } else if (grepl("\\blimited liability partnership\\b$", 
                   company, ignore.case = TRUE)) 
  {
    options <- c(options, gsub("\\blimited liability partnership\\b", "llp", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\blimited liability partnership\\b", "", 
                               company, ignore.case = TRUE))
  }
  # Limited Partnership
  if (grepl("\\blp\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\blp\\b", "limited partnership", company, 
                               ignore.case = TRUE))
    options <- c(options, gsub("\\blp\\b", "", company, ignore.case = TRUE))
  } else if (grepl("\\blimited partnership\\b$", company, ignore.case = TRUE)) 
  {
    options <- c(options, gsub("\\blimited partnership\\b", "lp", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\blimited partnership\\b", "", 
                               company, ignore.case = TRUE))
  }
  # Industrial and provident Society
  if (grepl("\\bips\\b$", company, ignore.case = TRUE)) {
    options <- c(options, gsub("\\bips\\b", "Industrial and Provident Society", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bips\\b", "", company, ignore.case = TRUE))
  } else if (grepl("\\bIndustrial and Provident Society\\b$", company, 
                   ignore.case = TRUE)) 
  {
    options <- c(options, gsub("\\bIndustrial and Provident Society\\b", "ips", 
                               company, ignore.case = TRUE))
    options <- c(options, gsub("\\bIndustrial and Provident Society\\b", "", 
                               company, ignore.case = TRUE))
  }
  
  # Ensure unique options
  options <- unique(options)
  
  # Setting up for number search
  x = length(options)
  results = data.frame(search = options, company_name = rep(NA, x), 
                       company_number = rep(NA, x), 
                       status = rep(NA, x ))
  
  # Search for matches across variations
  api_key = api_key
  
  for(mu in 1:x){
    company = options[mu]
    results[mu,2:4] = company_name_search(api_key, company)
  }
  # only keep positive matches
  positive = subset(results, company_number != "no match")
  return(positive)
  
}

# Function to extract company officer information from the company number
officer_info = function(api_key, company_number){
  
  headers = c(
    api_key = api_key,
    'Authorization' = 'Basic YTdkOWMzYjYtMjY3ZC00YmY1LWIwNzAtMmM3ODA4MzlkNTI5Og=='
  )
  url = paste0("https://api.company-information.service.gov.uk/company/", 
               company_number, "/officers")
  res <- VERB("GET", url = url, add_headers(headers))
  response_list <- fromJSON(content(res, 'text'), flatten = TRUE)
  response_df = response_list$items
  
  if ((is.list(response_df) && length(response_df) == 0) || 
      (is.data.frame(response_df) && nrow(response_df) == 0)) {
    response_df = data.frame(return = "N/A")
    return(response_df)
  }else{
    
    if("resigned_on" %in% names(response_list)){
      resigned_on = response_df$resigned_on
    }else{
      resigned_on = "N/A"
    }
    
    directors = data.frame(
      name = response_df$name,
      nationality = response_df$nationality,
      country_of_residence = response_df$country_of_residence,
      role = response_df$officer_role,
      date_appointed = response_df$appointed_on,
      date_resignation = resigned_on,
      original_occupation = response_df$occupation
    ) 
    
    # Registered Address
    address = data.frame(
      response_df$address.address_line_1,
      response_df$address.locality,
      response_df$address.postal_code
    )
    combined_rows = apply(address, 1, function(x) paste(x, collapse = ", ")) 
    
    directors$registered_address = combined_rows
    
    # Date of Birth
    dob = data.frame(
      response_df$date_of_birth.month,
      response_df$date_of_birth.year
    )
    combined_rows = apply(dob, 1, function(x) paste(x, collapse = "-")) 
    
    directors$dob = combined_rows
    
    directors[is.na(directors)] <- ""
    
    return(directors)
  }
}

# Function to extract Person with Significant Control information from the 
# company number

psc_info = function(api_key, company_number){
  
  headers = c(
    api_key = api_key,
    'Authorization' = 'Basic YTdkOWMzYjYtMjY3ZC00YmY1LWIwNzAtMmM3ODA4MzlkNTI5Og=='
  )
  url = paste0("https://api.company-information.service.gov.uk/company/", 
               company_number, "/persons-with-significant-control")
  res <- VERB("GET", url = url, add_headers(headers))
  response_list <- fromJSON(content(res, 'text'), flatten = TRUE)
  response_df = response_list$items
  
  # Check if response_df is an empty list or empty dataframe
  if ((is.list(response_df) && length(response_df) == 0) || 
      (is.data.frame(response_df) && nrow(response_df) == 0)) {
    response_df = data.frame(return = "N/A")
    return(response_df)
    stop()
  }else{
    
    # DOB
    if("date_of_birth.month" %in% names(response_df)){
      if("date_of_birth.year" %in% names(response_df)){
        dob = data.frame(
          response_df$date_of_birth.month,
          response_df$date_of_birth.year
        )
        combined_rows = apply(dob, 1, function(x) paste(x, collapse = "-")) 
        dob = data.frame(date_of_birth = combined_rows) 
      }else{dob = rep("N/A", nrow(response_df))}
    }else{dob = rep("N/A", nrow(response_df))}
    
    # Control
    if(is.list(response_df$natures_of_control)){
      control <- sapply(response_df$natures_of_control, 
                        function(x) paste(x, collapse = ", '"))
    }
    
    # Date of cessation
    if("ceased_on" %in% names(response_df)){
      date_ceased = response_df$ceased_on
    }else{
      date_ceased = rep("N/A", nrow(response_df))
    }
    
    # Nationality
    if("nationality" %in% names(response_df)){
      nationality = response_df$nationality
    }else{
      nationality = rep("N/A", nrow(response_df))
    }
    
    # Country of residence
    if("country_of_residence" %in% names(response_df)){
      country_of_residence = response_df$country_of_residence
    }else{
      country_of_residence = rep("N/A", nrow(response_df))
    }
    
    # Country of Address
    # Country of residence
    if("address.country" %in% names(response_df)){
      country_of_address = response_df$address.country
    }else{
      country_of_address = rep("N/A", nrow(response_df))
    }
    
    # PSC
    psc = data.frame(
      name = response_df$name,
      control = control,
      date_notified = response_df$notified_on,
      ceased = response_df$ceased,
      date_ceased = date_ceased,
      dob = dob,
      nationality = nationality,
      country_of_residence = country_of_residence,
      country_of_address = country_of_address
    )
    
    return(psc)
  }
}

# Function to extract the list of available documents from the company number

documents_available = function(api_key, company_number){
  
  headers = c(
    api_key = api_key,
    'Authorization' = 'Basic YTdkOWMzYjYtMjY3ZC00YmY1LWIwNzAtMmM3ODA4MzlkNTI5Og=='
  )
  url = paste0("https://api.company-information.service.gov.uk/company/", 
               company_number, "/filing-history")
  res <- VERB("GET", url = url, add_headers(headers))
  response_list <- fromJSON(content(res, 'text'), flatten = TRUE)
  
  response = response_list$items
  
  documents = data.frame(
    description = response$description,
    category = response$category,
    date = response$date,
    pages = response$pages,
    id = response$transaction_id
  )
  
  return(documents)
}

# Function to extract charges information from the company number

charges_info = function(api_key, company_number){
  
  headers = c(
    api_key = api_key,
    'Authorization' = 'Basic YTdkOWMzYjYtMjY3ZC00YmY1LWIwNzAtMmM3ODA4MzlkNTI5Og=='
  )
  url = paste0("https://api.company-information.service.gov.uk/company/", 
               company_number, "/charges")
  res <- VERB("GET", url = url, add_headers(headers))
  response_list <- fromJSON(content(res, 'text'), flatten = TRUE)
  
  response_df = response_list$items
  
  # Entitled Persons
  entitled_persons <- sapply(response_df$persons_entitled, function(df) {
    apply(df, 1, paste, collapse = ", ") %>% 
      paste(collapse = ", ")
  }
  )
  
  charges = data.frame(
    charge_type = response_df$classification.description,
    entitled_persons = entitled_persons,
    description = response_df$particulars.description,
    created = response_df$created_on,
    status = response_df$status,
    satisfied = response_df$satisfied_on
  )
  
  return(charges)
}

# Function to extract insolvency information from the company number

insolvency_info = function(api_key, company_number){
  
  headers = c(
    api_key = api_key,
    'Authorization' = 'Basic YTdkOWMzYjYtMjY3ZC00YmY1LWIwNzAtMmM3ODA4MzlkNTI5Og=='
  )
  url = paste0("https://api.company-information.service.gov.uk/company/", 
               company_number, "/insolvency")
  res <- VERB("GET", url = url, add_headers(headers))
  response_list <- fromJSON(content(res, 'text'), flatten = TRUE)
  
  response_df = response_list$cases
  
  x = nrow(response_df)
  
  type = rep(NA, x)
  practitioners = rep(NA, x)
  dates = as.data.frame(matrix(NA, ncol = 2, nrow = x))
  
  
  for(i in 1:x){
    # Type of case
    if("type" %in% names(response_df)){
      type[i] = response_df$type[i]
    }else{
      type[i] = "N/A"
    }
    
    # Practitioners
    if("practitioners" %in% names(response_df)){
      p = as.data.frame(response_df$practitioners[1])
      practitioners[i] = paste(p$name, collapse = ", ")
    }else{
      practitioners[i] = "N/A"
    }
    
    # Dates of case
    if("dates" %in% names(response_df)){
      d = as.data.frame(response_df$dates[1])
      t = t(d[,2, drop=FALSE])
      dates[i,] = as.data.frame(t)
      colnames(dates) = d[,1]
    }else{
      dates[i,] = rep("N/A", 2)
    }
  }
  
  insolvency = data.frame(
    type, 
    practitioners
  )
  
  insolvency = cbind(insolvency, dates)
  
  return(insolvency)
}

# Function to extract general company information from the company number

general_info = function(api_key, company_number, sic){
  
  headers = c(
    api_key = api_key,
    'Authorization' = 'Basic YTdkOWMzYjYtMjY3ZC00YmY1LWIwNzAtMmM3ODA4MzlkNTI5Og=='
  )
  url = paste0("https://api.company-information.service.gov.uk/company/", 
               company_number)
  res <- VERB("GET", url = url, add_headers(headers))
  response_list <- fromJSON(content(res, 'text'), flatten = TRUE)
  
  if ((is.list(response_list) && length(response_list) == 0) || 
      (is.data.frame(response_list) && nrow(response_list) == 0)) {
    response_df = data.frame(return = "N/A")
    return(response_df)
  }else{
    
    # Office Address
    if("registered_office_address" %in% names(response_list)){
      office = as.data.frame(t(as.data.frame(
        response_list$registered_office_address)
      ))
      rows = 1:nrow(office)
      entries = office[rows, 1]
      registered_office = paste(entries, collapse = ", ")
    }
    
    # Industry
    if("sic_codes" %in% names(response_list)){
      x = which(response_list$sic_codes == sic[,1])
      if(is.integer(x) && length(x) == 0){
        industry = paste0(response_list$sic_codes, 
                          "- no match on concentrated codes list")
      }else{
        industry = sic$Description[x]
      }
    }else{
      industry = "N/A"
    }
    
    # Liquidation
    if("has_been_liquidated" %in% names(response_list)){
      liquidated = ifelse(response_list$has_been_liquidated == FALSE, 
                          "N/A", "Yes")
    }else{
      liquidated = "N/A"
    }
    
    # Charges
    if("has_charges" %in% names(response_list)){
      charges = ifelse(response_list$has_charges == FALSE, "N/A", "Yes")
    }else{
      charges = "N/A"
    }
    
    # Insolvency
    if("has_insolvency_history" %in% names(response_list)){
      insolvency = ifelse(response_list$has_insolvency_history == FALSE, 
                          "N/A", "Yes")
    }else{
      insolvency = "N/A"
    }
    
    # Jurisdiction
    if("jurisdiction" %in% names(response_list)){
      jurisdiction = response_list$jurisdiction
    }else{
      jurisdiction = "N/A"
    }
    
    # Previous names
    if("previous_company_names" %in% names(response_list)){
      
      # Previous Names
      names = response_list$previous_company_names[, c(3,2,1)]
      rows = 1:nrow(names)
      entries = names[rows, 1] 
      previous_names = paste(entries, collapse = ", ")
      
      # Current Name
      name_dates = as.vector(unlist(names[, c(2,3)]))
      name_dates = as.Date(name_dates)
      current_name_date = max(name_dates)
      
    }else{
      previous_names = "N/A"
      current_name_date = response_list$date_of_creation
    }
    
    # Dissolution date
    if("date_of_cessation" %in% names(response_list)){
      dissolution_date =  response_list$date_of_cessation
    }else{
      dissolution_date = "N/A"
    }
    
    # Officers
    officers = officer_info(api_key, company_number)
    officers = paste(officers$name, collapse = "; ")
    
    # psc
    psc = psc_info(api_key, company_number)
    psc = paste(psc$name, collapse = "; ")
    
    # Output
    response_df = data.frame(
      company_name = response_list$company_name,
      company_number = response_list$company_number,
      incorporation_date = response_list$date_of_creation,
      status = response_list$company_status,
      dissolution_date = dissolution_date,
      industry = industry,
      previous_names = previous_names,
      date_current_name = current_name_date,
      jurisdiction = jurisdiction,
      registered_office = registered_office,
      liquidated = liquidated,
      charges = charges,
      insolvency = insolvency,
      officers = officers,
      psc = psc
    )
    
    general = as.data.frame(t(response_df))
    general <- cbind(RowName = rownames(general), general)
    rownames(general) <- NULL
    colnames(general) = NULL
    
    return(general)
    
  }
}

pie = function(variable, i){
  
  df <- as.data.frame(table(variable))
  colnames(df) <- c("Category", "Count")
  title = c("Distribution of Company Status", "Has the company been liquidated?",
            "Does the company have charges?", "Is the company insolvent?")
  
  df$Percentage <- round(df$Count / sum(df$Count) * 100, 1)  # Round to 1 decimal place
  df$Label <- paste0(df$Category, "\n", df$Percentage, "%")  # Label with name + %
  
  # Pie chart with percentage labels
  pie = ggplot(df, aes(x = "", y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1) + 
    coord_polar(theta = "y") + 
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, color = "white") + 
    labs(title = title[i]) +
    scale_fill_brewer(palette = "Set1") +  # You can change color palette
    theme_void() +
    theme(legend.position = "none") +
    xlab("")
  
  return(pie)
}

hist = function(variable, i){
  
  df <- as.data.frame(table(variable))
  colnames(df) <- c("Category", "Count")
  title = c("Distribution of Company Status", 
            "Has the company been 
            liquidated?",
            "Does the company have 
            charges?", 
            "Is the company insolvent?")
  
  df$Label <- paste0(df$Category, "\n", df$Count)
  
  hist = ggplot(df, aes(x = Category, y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = Count), vjust = -0.5, size = 5) +  
    labs(title = title[i], y = "Count") +
    scale_fill_brewer(palette = "Set1") +  
    theme_minimal() +  
    theme(legend.position = "none") +
    xlab("")
  
  return(hist)
}

barplot = function(variable,i){
  
  df = as.data.frame(table(trimws(unlist(strsplit(variable, ";")))))
  colnames(df) = c("Category", "Count")
  
  if(nrow(df) > 15){
    df = df[order(df$Count,decreasing = T),]
    df = df[1:15,]
  }
  
  title = c("Distribution of Persons with Significant Control",
            "Distribution of Officers")
  x = c("Persons with Significant Control", "Officers")
  
  # Create a bar chart to visualize distribution
  ggplot(df, aes(x = Category, y = Count)) +
    geom_bar(stat = "identity", width = 0.7) + 
    labs(title = title[i], 
         x = x[i], y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip()
}


extract = function(companies){
  
  # split the company names
  company_vector <- unlist(strsplit(companies, ", "))
  l = length(company_vector)
  
  # set up for the project output
  full = as.data.frame(matrix(nrow = 0, ncol = 15))
  colnames(full) = c("company_name", "company_number", "incorporation_date", 
                     "status", "dissolution_date", "industry", "previous_names", 
                     "date_current_name", "jurisdiction", "registered_office", 
                     "liquidated", "charges", "insolvency", "officers", "psc")
  
  # create output
  for(z in 1:l){
    
    company_name = company_vector[z]
    
    # Getting matches for the company number 
    positive = company_name_to_number(api_key, company_name)
    positive
    x = nrow(positive)
    
    if (x == 0) { 
      next 
    }else{
      
      for(lala in 1:x){
        
        # Extract individual sheet per company match
        company_number = positive$company_number[lala]
        
        # Extract company information 
        company_overview = general_info(api_key, company_number, sic)
        
        # adding to the project overview
        overview = company_overview
        overview = as.data.frame(t(overview))
        colnames(overview) = overview[1,]
        overview = overview[-1,]
        
        full = rbind(full, overview)
        
      }
    }
  }
  
  # date variables
  full$incorporation_date = as.Date(full$incorporation_date)
  full$dissolution_date = as.Date(full$dissolution_date)
  
  # industry categories
  index = grepl("no match", full$industry)
  full$industry[index] = "other"
  index = grepl("N/A", full$industry)
  full$industry[index] = "other"
  full$industry[is.na(full$industry)] = "other"
  
  # liquidated, charges, insolvency
  full$liquidated[full$liquidated != "Yes"] = "No"
  full$charges[full$charges!= "Yes"] = "No"
  full$insolvency[full$insolvency != "Yes"] = "No"
  
  return(full)
}

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  titlePanel("Trend Analysis of UK Companies"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text_input", 
                    "Enter the name of companies you would like to analyze:", 
                    "", rows = 10, width = "100%", placeholder = companies),
      actionButton("process_btn", "Search Companies"),
      br(),
      br(),
      selectizeInput("delete", "Manually delete companies (by number)*", choices = NULL, multiple = TRUE),
      p("* this deletes the companies from the unfiltered data."),
      actionButton("delete_btn", "Remove Companies"),
      hr(),
      h4("Apply Filters"),
      uiOutput("incorporation"),
      uiOutput("dissolution"),
      uiOutput("comment"),
      selectizeInput("status", "Filter by company status", choices = NULL, multiple = TRUE),
      selectizeInput("jurisdiction", "Filter by jurisdictions", choices = NULL, multiple = TRUE),
      selectizeInput("industry", "Filter by industry", choices = NULL, multiple = TRUE),
      fluidRow(
        column(4, selectizeInput("liquidated", "Liquidated", choices = c("Yes", "No"), select = NULL, multiple = TRUE)),
        column(4, selectizeInput("charges", "Has charges", choices = c("Yes", "No"), select = NULL, multiple = TRUE)),
        column(4, selectizeInput("insolvency", "Insolvent", choices = c("Yes", "No"), select = NULL, multiple = TRUE))       
      ),
      selectizeInput("officers", "Filter by company officers", choices = NULL, multiple = TRUE),
      selectizeInput("psc", "Filter by Persons with Significant Control", choices = NULL, multiple = TRUE),
      fluidRow(
        column(6, actionButton("filter_btn", "Apply Filters")),
        column(6, actionButton("unfilter_btn", "Reset Data"))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        # Raw Data
        tabPanel(
          "Raw Data",
          dataTableOutput("output_table")
        ),
        
        tabPanel(
          "Raw Data - Filtered",
          dataTableOutput("output_table2")
        ),
        
        # Trends
        tabPanel(
          "Trends",
          # Pie Charts
          fluidRow(
            column(3, plotOutput("status_pie")),
            column(3, plotOutput("liquidated_pie")),
            column(3, plotOutput("charges_pie")),
            column(3, plotOutput("insolvency_pie"))
          ),
          # Histograms
          fluidRow(
            column(3, plotOutput("status_hist")),
            column(3, plotOutput("liquidated_hist")),
            column(3, plotOutput("charges_hist")),
            column(3, plotOutput("insolvency_hist"))
          ),
          # Barplots
          fluidRow(
            column(6, plotOutput("psc_bar")),
            column(6, plotOutput("officer_bar"))
          )
        ),
        
        tabPanel(
          "Trends - Filtered",
          # Pie Charts
          fluidRow(
            column(3, plotOutput("status_pie2")),
            column(3, plotOutput("liquidated_pie2")),
            column(3, plotOutput("charges_pie2")),
            column(3, plotOutput("insolvency_pie2"))
          ),
          # Histograms
          fluidRow(
            column(3, plotOutput("status_hist2")),
            column(3, plotOutput("liquidated_hist2")),
            column(3, plotOutput("charges_hist2")),
            column(3, plotOutput("insolvency_hist2"))
          ),
          # Barplots
          fluidRow(
            column(6, plotOutput("psc_bar2")),
            column(6, plotOutput("officer_bar2"))
          )
        ),
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Set Up
  original <- reactiveVal(NULL)  
  update <- reactiveVal(NULL)  
  
  # (1) Pull raw information
  observeEvent(input$process_btn, {
    req(input$text_input)  
    df <- extract(input$text_input)
    
    original(df)  
    update(df)  
  })
  
  observeEvent(input$delete_btn, {
    req(original())
    req(input$delete)
    
    df = original()
    df = df[!df$company_number %in% input$delete,]
    
    original(df)
    update(df)
  })
  
  ## (1.a) Create time delimitation 
  output$incorporation = renderUI({
    req(original())
    df = original()
    
    fluidRow(
      column(6, dateInput("in_start", "Incorporated After:", 
                          min = min(df$incorporation_date, na.rm = T),
                          max = max(df$incorporation_date, na.rm = T),
                          value = min(df$incorporation_date, na.rm = T))),
      column(6, dateInput("in_end", "Incorporated Before:", 
                          min = min(df$incorporation_date, na.rm = T),
                          max = max(df$incorporation_date, na.rm = T),
                          value = max(df$incorporation_date, na.rm = T))),
    )
  })
  
  output$dissolution = renderUI({
    req(original())
    df = original()
    
    fluidRow(
      column(6, dateInput("dis_start", "Dissolved After:", 
                          min = min(df$dissolution_date, na.rm = T),
                          max = max(df$dissolution_date, na.rm = T),
                          value = min(df$dissolution_date, na.rm = T))),
      column(6, dateInput("dis_end", "Dissolved Before:*", 
                          min = min(df$dissolution_date, na.rm = T),
                          max = max(df$dissolution_date, na.rm = T),
                          value = max(df$dissolution_date, na.rm = T))),
    )
  })
  
  output$comment = renderUI({
    req(original())
    p("*to select companies dissolved before a certain date only, you need to select 'dissolved' under 'Filter by Company Status'.")
  })
  
  ## (1.b) Create category filters
  observe({
    req(original())
    df = original()
    
    s.choices = unique(trimws(df$status))
    updateSelectizeInput(session, "status", choices = s.choices, select = NULL, server = TRUE)
    
    i.choices = unique(trimws(df$industry))
    updateSelectizeInput(session, "industry", choices = i.choices, select = NULL, server = TRUE)
    
    j.choices = unique(trimws(df$jurisdiction))
    updateSelectizeInput(session, "jurisdiction", choices = j.choices, select = NULL, server = TRUE)
    
    o.choices = unique(trimws(unlist(strsplit(df$officers, ";"))))
    updateSelectizeInput(session, "officers", choices = o.choices, select = NULL, server = TRUE)
    
    psc.choices = unique(trimws(unlist(strsplit(df$psc, ";"))))
    updateSelectizeInput(session, "psc", choices = psc.choices, select = NULL, server = TRUE)
    
    d.choices = unique(trimws(as.character(df$company_number)))
    updateSelectizeInput(session, "delete", choices = d.choices, select = NULL, server = TRUE)
    
  })
  
  # (1.c) Apply Filters
  observeEvent(input$filter_btn, {
    
    req(original())  
    df = original()
    
    if(!is.null(input$in_start)){
      df = df[df$incorporation_date >= input$in_start,] 
    }
    
    if(!is.null(input$in_end)){
      df = df[df$incorporation_date <= input$in_end,] 
    }
    
    if(!is.null(input$dis_start)){
      df = df[df$dissolution_date >= input$dis_start | is.na(df$dissolution_date),] 
    }
    
    if(!is.null(input$dis_end) && !"active" %in% input$status && !is.null(input$status)){
      df = df[df$dissolution_date <= input$dis_end,] 
    }
    
    if(!is.null(input$jurisdiction)){
      df = df[df$jurisdiction %in% input$jurisdiction,]
    }
    
    if(!is.null(input$status)){
      df = df[df$status %in% input$status,]
    }
    
    if(!is.null(input$industry)){
      df = df[df$industry %in% input$industry,]
    }
    
    if(!is.null(input$liquidated)){
      df = df[df$liquidated %in% input$liquidated,]
    }
    
    if(!is.null(input$charges)){
      df = df[df$charges %in% input$charges,]
    }
    
    if(!is.null(input$insolvency)){
      df = df[df$insolvency %in% input$insolvency,]
    }
    
    if(!is.null(input$officers)){
      indices = which(sapply(input$officers, function(x) grepl(x, df$officers)))
      indices = unique(indices)
      df = df[indices,]
    }
    
    if(!is.null(input$psc)){
      indices = which(sapply(input$psc, function(x) grepl(x, df$psc)))
      indices = unique(indices)
      df = df[indices,]
    }
    
    update(df)
    
  })
  
  observeEvent(input$unfilter_btn, {
    req(original())
    df = original()
    update(df)
  })
  
  # (1.d) output data, raw and filtered
  output$output_table <- renderDataTable({
    req(original())  
    original()
  })
  
  output$output_table2 <- renderDataTable({
    req(update())  
    update()
  }) # End of (1) 
  
  # Summary Statistics
  output$status_pie = renderPlot({
    req(original())
    df = original()
    pie(df$status, 1)
  })
  
  output$liquidated_pie = renderPlot({
    req(original())
    df = original()
    pie(df$liquidated, 2)
  })
  
  output$charges_pie = renderPlot({
    req(original())
    df = original()
    pie(df$charges, 3)
  })
  
  output$insolvency_pie = renderPlot({
    req(original())
    df = original()
    pie(df$insolvency, 4)
  })
  
  output$status_hist = renderPlot({
    req(original())
    df = original()
    hist(df$status, 1)
  })
  
  output$liquidated_hist = renderPlot({
    req(original())
    df = original()
    hist(df$liquidated, 2)
  })
  
  output$charges_hist = renderPlot({
    req(original())
    df = original()
    hist(df$charges, 3)
  })
  
  output$insolvency_hist = renderPlot({
    req(original())
    df = original()
    hist(df$insolvency, 4)
  })
  
  output$officer_bar = renderPlot({
    req(original())
    df = original()
    barplot(df$officers, 2)
  })
  
  output$psc_bar = renderPlot({
    req(original())
    df = original()
    barplot(df$psc, 1)
  })
  
  # Summary Statistics - Filtered
  output$status_pie2 = renderPlot({
    req(update())
    df = update()
    pie(df$status, 1)
  })
  
  output$liquidated_pie2 = renderPlot({
    req(update())
    df = update()
    pie(df$liquidated, 2)
  })
  
  output$charges_pie2 = renderPlot({
    req(update())
    df = update()
    pie(df$charges, 3)
  })
  
  output$insolvency_pie2 = renderPlot({
    req(update())
    df = update()
    pie(df$insolvency, 4)
  })
  
  output$status_hist2 = renderPlot({
    req(update())
    df = update()
    hist(df$status, 1)
  })
  
  output$liquidated_hist2 = renderPlot({
    req(update())
    df = update()
    hist(df$liquidated, 2)
  })
  
  output$charges_hist2 = renderPlot({
    req(update())
    df = update()
    hist(df$charges, 3)
  })
  
  output$insolvency_hist2 = renderPlot({
    req(update())
    df = update()
    hist(df$insolvency, 4)
  })
  
  output$officer_bar2 = renderPlot({
    req(update())
    df = update()
    barplot(df$officers, 2)
  })
  
  output$psc_bar2 = renderPlot({
    req(update())
    df = update()
    barplot(df$psc, 1)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
