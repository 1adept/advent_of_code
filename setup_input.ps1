
Param (
	[Parameter(Mandatory, HelpMessage = "Need Token ...")]
	[string]$Token,
	[Parameter(Mandatory, HelpMessage = "Which year ?")]
	[int]$Year,
	[Parameter(Optional, HelpMessage = "Day?")]
	$Days = $(@(1..25))
)

$ThisYear = (Get-Date -Format yyyy)
if ($Year -ge $ThisYear) {
	Write-Error "Cannot get Data for this Year because it didnt happen yet"
	exit 
} elseif ($Year -eq $ThisYear) {
	Write-Host "Only getting todays data because its this year and we dont want to stress the server too bad :)"
	$Days = @((Get-Date -Format dd))
}

if (-Not(Test-Path "./$Year")) {
	mkdir "$Year"
}
cd "$Year"

Write-Host "Getting input for Advent of Code year $Year day $Day"

$BasePath = "./input/"
if (-Not (Test-Path $BasePath)) {
	mkdir input
}
cd input

$Session = New-Object Microsoft.PowerShell.Commands.WebRequestSession
$cookie  = New-Object System.Net.Cookie('session', "$Token",'/', 'adventofcode.com')
$Session.Cookies.Add($cookie)

$URL = "https://adventofcode.com/$Year/day/"
foreach ($d in $Days) {
	$name = "day$($d.ToString('00')).in"
	if (Test-Path $name) {
		continue
	}
	$u = $URL + "$d/input"
	Invoke-WebRequest $u -UseBasicParsing -WebSession $Session | ni $name
	Write-Host "Created $name for day $d"
}
cd ..
cd ..


