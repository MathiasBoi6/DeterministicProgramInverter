Green='\033[0;32m'
NC='\033[0m'

printf "Compiling scripts!\n"
fsharpc --target:library --nologo --out:RobNFA.dll RobNFA.fs
printf "${Green}Done with NFADefiner ${NC}\n"
fsharpc --target:library --nologo -r:RobNFA.dll --out:RobDFA.dll RobDFA.fs
printf "${Green}Done with DFADefiner ${NC}\n"
printf "${NC}Finished \n"
