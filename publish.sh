
sshGitHub="git@github.com:imastaser/imastaser.github.io.git"
hakylldir=~/imastaser.github.io
pubdir=$hakylldir/_publish

./rebuild.sh        && \
echo -- "Change dir to _publish" && \
cd $pubdir && pwd \
echo -- "Remove all files except .git" && \
git rm -rf . && \
git clean -fxd  && \
git reset && \
echo -- "Copy _site to ." && \
cp -rf ../_site/* . && \
echo -- "Adding files to repository" && \
git add . && \
git add -u && \
echo -- "Commit and publish" && \
git commit -m "publishing"
git push --force origin master
