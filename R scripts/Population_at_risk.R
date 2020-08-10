download.file('https://www.local.gov.uk/sites/default/files/documents/Copy%20of%20COVID%20-%2019%20Estimated%20Population%20at%20risk%20by%20LA.xlsx', paste0(github_repo_dir, '/lg_inform_pop_at_risk.xlsx'), mode = 'wb')

lg_inform_pop_at_risk <- read_excel("~/Documents/Repositories/another_covid_repo/lg_inform_pop_at_risk.xlsx", sheet = "Data", skip = 3)