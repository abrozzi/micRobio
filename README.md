# microbio

Git and Rstudio build-package installation and configuration:
---

1 - Create by Rstudio a project to build a package:

  ~/ab/mypack

2 - Create a repository on GitHub

3 - Move to the local package directory and:

  cd ~/ab/mypack

  git init
  
  git add .

  git commit -m "first commit"

  git remote add origin https://github.com/ab/mypack.git

  git push -u origin master

4 - In Rstudio you can do the same by Git menu: commit -> push-

5 - If you need proxy add in your home a file .Renviron with the following lines:

  http_proxy="http://myproxy.net:port"

  https_proxy="https://myproxy.net:2011"
