
sshGitHub="git@github.com:imastaser/imastaser.github.io.git"
hakylldir=~/imastaser.github.io
pubdir=$hakylldir/_publish

# [[ ! -e $pubdir ]] && git clone -b master $sshGitHub --single-branch _publish

#./rebuild.sh        && \
echo -- "cahnge dir"
cd $pubdir          && pwd \
# git checkout master && \
echo -- "get latest modif from github" && \
git pull origin master && \
echo -- "Remove all files except .git" && \
#\rm -rf * && \
echo -- "Copy _site" && \
\cp -rf ../_site/* . && \
echo -- "Adding files to repository" && \
git add . && \
git add -u && \
echo -- "Commit and publish" && \
git commit -m "publishing"
git push origin master
