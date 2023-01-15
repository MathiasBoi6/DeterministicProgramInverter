# Fsharp inverter

## To use the inverter with the webhost
1. Open the Fable directory in your terminal.
2. Then use the command 'npm start'. 
3. If all goes well the website should be hosted at localhost:8080.
<br>
For examples of how to write fct programs, grammar programs and input stacks, 
<br>
please check the Tests/ directory.
<br>
<br>

## To use the inverter without the webhost, one can use the following commands:
<br>
dotnet run inverting "fctFile" "StackFile"
<br>
&nbsp; &nbsp; &nbsp;	Inverts and interprets inverse
<br><br>
dotnet run inverting -Invert "fctFile" 
<br>
&nbsp; &nbsp; &nbsp;	Inverts and prints inverse grammar
<br><br>
dotnet run inverting -InvertComplete "fctFile" 
<br>
&nbsp; &nbsp; &nbsp;	Inverts and prints inverse grammar before and after transition compression
<br><br>
dotnet run inverting -LexStack "StackFile"
<br>
&nbsp; &nbsp; &nbsp;	Prints stack
<br><br>
dotnet run inverting -Interpret "fctFile" "StackFile"
<br>
&nbsp; &nbsp; &nbsp;	Interprets without inverting
<br><br>
dotnet run inverting -Test fct
<br>
&nbsp; &nbsp; &nbsp;	Tests forwards and backwards computation of all fct files in directory Tests/
<br><br>
dotnet run inverting -DoubleInvert"fctFile" 
<br>
&nbsp; &nbsp; &nbsp;	Prints grammar and its doubly inverted counterpart.