library(usethis)

# Follow README at https://github.com/r-lib/usethis

# Creates following files/directories:
# - .Rbuildignore
# - .Rproj
# - DESCRIPTION
# - man/
# - NAMESPACE
# - R/
create_package(path = "/Users/pdiakumis/Desktop/projects/pdiakumis/woofr")

# - Sets License field in DESCRIPTION
# - Creates LICENSE and LICENSE.md files
use_mit_license(name = "Peter Diakumis")

# - Include a package in the DESCRIPTION under e.g. Imports (default)
use_package("dplyr", type = "Imports")
use_package("ggplot2")
use_package("purrr")
use_package("tibble")
use_package("tidyr")

# - Set Roxygen field in DESCRIPTION to 'list(markdown = TRUE)'
# - Set RoxygenNote field in DESCRIPTION to '6.1.1'
use_roxygen_md()

# - Generates README.Rmd so that you can mingle code with prose
use_readme_rmd()

# - Generates NEWS.md to keep track of new features added or major issues resolved
use_news_md()

# - Adds 'testthat' to Suggests field in DESCRIPTION
# - Creates 'tests/testthat/'
# - Writes 'tests/testthat.R'
# - Writes 'tests/testthat/test-my-test.R'
use_test("my-test1")

# Allows you and users to use '%>%' without explicitly loading dplyr/magrittr
# Adds 'magrittr' to Imports field in DESCRIPTION
# Writes 'R/utils-pipe.R'
use_pipe()

# set up git if not already initialised repo
use_git()

### Tips

# load pkg: Cmd + Shift + L
# build pkg: Cmd + Shift + B
# check pkg: Cmd + Shift + E
# doc pkg: Cmd + Shift + D
