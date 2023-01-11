Green='\033[0;32m'
NC='\033[0m'

printf "Compiling scripts!\n"
fsharpc --target:library --nologo --out:NFADefiner.dll NFADefiner.fs
printf "${Green}Done with NFADefiner ${NC}\n"
fsharpc --target:library --nologo -r:NFADefiner.dll --out:DFADefiner.dll DFADefiner.fs
printf "${Green}Done with DFADefiner ${NC}\n"
printf "${NC}Finished \n"
