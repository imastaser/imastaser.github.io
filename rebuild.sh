
# echo -- "start ssh agent"
# eval "$(ssh-agent -s)"
# ssh-add ~/.ssh/id_rsa

echo -- "Compiling and building"
cd ./web
stack exec imastaser clean && \
stack exec imastaser build
