Calculate distances between Sydney train stations according to the Ticketing Customer Handbook.

# Install and run locally

Install Haskell Platform
cabal update
git clone git://github.com/mzhou/opaldist.git
cd opaldist
cabal install
./dist/build/opaldisthttp/opaldisthttp 6725
Browse to eg. http://localhost:6725/train/Artarmon/TownHall

# Deploy on Heroku

heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master

