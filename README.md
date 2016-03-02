# microbio

[How to] (https://github.com/abrozzi/microbio#how-to)

* [Git and RStudio](https://github.com/abrozzi/microbio#git-rstudio)
* [Install pyclone] (https://github.com/abrozzi/microbio#pyclone-inst)

How to
====

### Git and RStudio 

1 - Create by RStudio a project to build a package:

> ~/ab/mypack

2 - Create a repository on GitHub

3 - Move to the local package directory and:

> cd ~/ab/mypack

> git init
  
> git add .

> git commit -m "first commit"

> git remote add origin https://github.com/ab/mypack.git

> git push -u origin master

4 - In Rstudio you can do the same by Git menu: commit -> push-

5 - If you need proxy add in your home a file .Renviron with the following lines:

> http_proxy="http://myproxy.net:port"

> https_proxy="https://myproxy.net:2011"

### Install pyclone

'export http_proxy=http://chbs-dmz-proxy01.nibr.novartis.net:2011'

'export https_proxy=https://chbs-dmz-proxy01.nibr.novartis.net:2011'

wget https://repo.continuum.io/miniconda/Miniconda-latest-MacOSX-x86_64.sh

bash Miniconda-latest-MacOSX-x86_64.sh

/Users/ab/miniconda2/bin/./conda install numpy

/Users/ab/miniconda2/bin/./conda install pyclone -c aroth85

# RUN

sudo PyClone run_analysis_pipeline --in_files Model1_ploidy1_PyCloneInput_primary.tsv --working_dir /Users/ab/marghe/

sudo PyClone plot_loci --config_file config.yaml --plot_file Model1_ploidy1_PyCloneInput_primary_PLOT --plot_type density
