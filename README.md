# pdaprules

The `pdaprules` package was created to help provide documentation on working with PRIME systems Posit Workbench and Posit Connect. We provide helpful functions for connecting to S3 as well as deploying and working with shiny applications in our connect environment. 

To get started, install the pdaprules package using the following code snippet (make sure devtools is installed):

```R
install.packages('devtools')
devtools::install_github(repo = "https://github.com/pepfar-datim/pdaprules.git", ref = "main")
```

You can reference the following vignettes depending on your objectives:

## Posit Connect Community Apps

You can reference the `package_vignette_apps.Rmd` under `vignettes/` for relevant documentation on how to use this package if you are a developer deploying an application as part of our community apps program.

## Posit Workbench Developers

You can reference the `package_vignette_developers.Rmd` under `vignettes/` for relevant documentation on how to use this package if you are a developer working primarily in workbench.
