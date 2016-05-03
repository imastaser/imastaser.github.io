---
title: Hakyll, GitHub repository and GitHub pages set up
date: 2014-02-01
description: DESCRIPTION
comments: true
toc: true
---

##GitHub preparation

1. create git repository "byurakn"  - for holding source
2. create git repository "byruakn.am" - for static generated stuff

##Hakyll Stuff

#### Creating hakyll site

1. cd to folder
2. hakyll-init my-site
    This creates a folder my-site in the current directory, with some example
    content and a generic configuration.
2.1 add "CNAME" file  

  * ```cabal update```
  * ```cabal init``` -- create config file and edit it to set up depends package if need, and set main-is:
  * ```cabal sandbox init```
  * ```cabal install```
  * ```./site clean```
  * ```./site watch --port=5000```

#### git setup

 * ```git config --system  --list```  -- show configs from  /usr/etc/gitconfig
 * ```git config --global  --list```  -- show configs from  ~/.gitconfig
 * ```git config --local   --list```     -- show configs from .git/config
 * ```git config --list```            -- show
 * ```git remote -v```             --  show remote repository address if exist
 * ```git remote add origin https://github.comarthurvard/byurakn.git```  --add address with alias origin
 * ```git branch -a```  -- show branches


3. ```cd my-site```
4. ```git init```
5. ```git remote add origin https://github.comarthurvard/byurakn.git```
6. ```git pull origin master``` *this step is required when repository is not empty, usually ther is READ.me file
7. add .gitignore file for hakyll
8. ```git add .```
8. ```git commit -m "init commit"```
9. ```git push -u origin master```


#### [GitHub pages]()

 * ```cd my-site``` --  if need
 * ```mkdir _publish```
 * ```cd _publish```
 * ```git init```
 * ```git remote -v```
 * ```git remote add origin https://github.com/ArthurVard/byurakn.am```
 * ```cp ../CNAME .```
 * ```git add CNAME```
 * ```git commit -m "CNAME file"```
 * ```git checkout --orphan gh-pages```
 * ```git push -u origin gh-pages``` -- create branch
 * ``` add publish.sh and rebuild.sh script files to current dir```
 * ``` emcas publish.sh``` -- change hakyll-site dir and git repo address
 * ```./publish.sh```
