# Aug 
#rm( list = ls())

#Connect to github repo
#install.packages("gitcreds")
usethis::create_github_token()
gitcreds::gitcreds_set()

usethis::gh_token_help()
usethis::git_sitrep()
gh::gh_whoami()