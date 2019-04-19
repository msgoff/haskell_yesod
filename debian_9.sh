
#build-from-source 
#http://postgrest.org/en/v5.2/install.html 
git clone https://github.com/PostgREST/postgrest.git 
cd postgrest 

# adjust local-bin-path to taste 
stack build --install-ghc --copy-bins --local-bin-path /usr/local/bin 
