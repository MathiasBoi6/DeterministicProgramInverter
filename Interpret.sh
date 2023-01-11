file="$1"
echo "Interpreting $file";
dotnet run inverting "-Interpret" "Tests/${file}/${file}.fct" "Tests/${file}/${file}.stackInp"
