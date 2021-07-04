# IMEXHUB
Our goal is to support businesses and charities that lack the technical resources to work with multiple sources of data efficiently. Without us they either struggle with a manual, repetitive process, or use a third party software vendor and pay transaction based subscription fees. We offer either a one-off implementation cost, or a free “do it yourself” option.


## WHO’S IT FOR?
- Organisations looking to automate the inefficient repetitive work necessary to bridge the gap between systems

- Professionals wanting to learn/retrain, while solving real life problems with the structure and support to create, showcase and scale their solution


## WHY OPEN SOURCE?
- Common features required across data connection/transformation projects make solutions that were previously uneconomical to develop from scratch (for charities/SMEs) more likely to be created

- Wide variety of input and output systems with a need for connection, such as invoices/apps/reports/document management systems


## WHAT CAN IMEXHUB DO?
We're excited that the first iteration of IMEXHUB is already helping two organisations with their business admin processes:

- A CHARITY uses IMEXHUB to build custom reports for a self-managed invoicing process
    - MONTHLY REPORTS PROCESSED SINCE AUGUST 2020 - Less customer questions for improved satisfaction ratings
    - 100+ HOURS SAVED ACROSS 3 TEAMS (PER REPORT) - High employee and volunteer morale working with accurate data
    - 2,000+ LINES OF DATA PROCESSED IN 12 SECONDS - Increased funds with reliable account management

- STATEMENTREADER (our sibling product) uses IMEXHUB to convert multi currency PayPal CSV data into Quickbooks
    - LOCAL DATA PROCESSING - No third party API authorisation required, no loss of your customer data, no data limits
    - USER STAYS IN CONTROL - See the transactions in Excel before uploading to Quickbooks
    - FREE - Completely:) The closest comparison product costs £hundreds each year to process more than 100 transactions per month


## GETTING STARTED (LOCAL IMPLEMENTATION)
#### R and RStudio setup
```
install.packages(c("shiny", "shinyWidgets", "shinythemes", "shinyFiles", "shinyjs", "config", "tidyverse", "lubridate", "readxl", "openxlsx", "DT", "mongolite", "jsonlite", "RSQLite", "cachem"))
```

#### MongoDB setup
Create a collection called "imexhub_collection", and a database called "imexhub_database"

#### Application setup
Create config.yaml:
```
default:
    host: ""
    port: ""
    username: ""
    password: ""

local:
    host: "127.0.0.1"
    port: "27017"
    username: ""
    password: ""
```
[15/5 more coming soon]


## GETTING STARTED (SERVER IMPLEMENTATION)
[14/5 more coming soon]


## WHAT'S NEXT?
- Documentation for developers to contribute and for clients to implement the application
- Cards, for separate input and export configurations
- Sessions, for concurrent users
- Email (input)
- PDF QR code (input)
- Duplicate transactions (process)
- PDF text parser (input)
- Schedule imports
- Schedule exports
- Renderui to reset fileInput on import file
- Document history log
- User settings window
- Display whether import is already exported in overview table
- SEO analysis for feature list and homepage optimisation

## GET IN TOUCH
Email us at david@statementreader.com or connect here: https://www.linkedin.com/in/delms/
