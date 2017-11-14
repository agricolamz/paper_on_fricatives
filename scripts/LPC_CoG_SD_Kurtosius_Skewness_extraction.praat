# This is a Praat script for extracting smothed LPC coordinates for all annotated files in directory. The sound annotation should be on the first Tier, the word information should be on the second Tier.

# read all files from the selected directory -------------------------------------------------------

# form Open all files in directory
#  comment Directory of sound files
#  text directory /home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 I s (with Inna Sieber)/sound/
#  comment Where do you want to save the results?
#  text resultfile /home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 I s (with Inna Sieber)/data/LPC_results.csv
#  comment On the end of its work this script remove all files from the object window! Be careful!
#endform

directory$ = "/home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 II s (with Inna Sieber)/sound/"
resultfile$  = "/home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 II s (with Inna Sieber)/data/LPC_results.csv"
cogfile$  = "/home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 II s (with Inna Sieber)/data/CoG_results.csv"
header$ = "soundname" + "'tab$'" + "value" +"'tab$'"+"cog"+"'tab$'"+"sd"+"'tab$'"+"skewness"+"'tab$'"+"kurtosis"+"'newline$'"
fileappend "'cogfile$'" 'header$'

Create Strings as file list: "list", directory$ + "/*.wav"
numberOfFiles = Get number of strings

for ifile to numberOfFiles
	filename$ = Get string... ifile
	Read from file... 'directory$''filename$'
	soundname$ = selected$ ("Sound", 1)
	gridfile$ = "'directory$''soundname$'.TextGrid"
	Read from file... 'gridfile$'

# extract labels from the second Tier -------------------------------------------------------------

	selectObject: "TextGrid " + soundname$ - ".TextGrid" - ".wav"
	n_intervals = Get number of intervals: 1
	n_intervals = (n_intervals - 1)/2
	for i from 1 to n_intervals
		value$ [i] = Get label of interval: 2, i*2
	endfor

# extract fragments and convert to LPC ----------------------------------------------------------

	plusObject: "Sound " + soundname$ - ".TextGrid" - ".WAV"
	Extract non-empty intervals: 1, "no"
	To Spectrum: "yes"
	LPC smoothing: 5, 50

# extract LPC and write to the file -----------------------------------------------------------------
	n = numberOfSelected ("Spectrum")
	for i to n
		spectrum [i] = selected ("Spectrum", i)
	endfor

	for i to n
		selectObject: spectrum [i]
		data$ = List: "no", "yes", "no", "no", "no", "yes"
		data$ = replace$ (data$, "'newline$'", "'tab$'"+soundname$ + "'tab$'" + value$ [i] +"'newline$'", 0)
		fileappend "'resultfile$'" 'data$'
		cog$ = Get centre of gravity: 2
		sd$ = Get standard deviation: 2
		skewness$ = Get skewness: 2
		kurtosis$ = Get kurtosis: 2
		cog$ = replace$ (cog$, " hertz", "", 0)
		sd$ = replace$ (sd$, " hertz", "", 0)
		cog$ = soundname$ + "'tab$'" + value$ [i] +"'tab$'"+cog$+"'tab$'"+sd$+"'tab$'"+skewness$+"'tab$'"+kurtosis$+"'newline$'"
		fileappend "'cogfile$'" 'cog$'
	endfor
	select Strings list
endfor

# remove all created files ---------------------------------------------------------------------------

select all
Remove
