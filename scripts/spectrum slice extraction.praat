# read all files from the selected directory -------------------------------------------------------

form Open all files in directory
  sentence Directory /home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 I s (with Inna Sieber)/praat script/sound/
  comment Where do you want to save the results?
  text textfile /home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 I s (with Inna Sieber)/praat script/sound/result.txt
endform
Create Strings as file list... list 'directory$'*
numberOfFiles = Get number of strings
for ifile to numberOfFiles
	filename$ = Get string... ifile
		# You can add some filename extensions that you want to be excluded to the next line.
		if right$ (filename$, 4) <> ".doc" and right$ (filename$, 4) <> ".xls" and right$ (filename$, 4) <> ".XLS" and right$ (filename$, 4) <> ".TXT" and right$ (filename$, 4) <> ".txt" and right$ (filename$, 4) <> ".dat" and right$ (filename$, 4) <> ".DAT"
			Read from file... 'directory$''filename$'
		endif
	select Strings list
endfor

# select first TextGrid files --------------------------------------------------------------------------------------

select Strings list
file$ = Get string: 1
selectObject: "TextGrid " + file$ - ".TextGrid" - ".WAV"

# extract labels from the second Tier ------------------------------------------------------------------------

n_intervals = Get number of intervals: 1
n_intervals = (n_intervals - 1)/2
for i from 1 to n_intervals
	value$ [i] = Get label of interval: 2, i*2
endfor

# extract fragments and convert to LPC ---------------------------------------------------------------------

plusObject: "Sound " + file$ - ".TextGrid" - ".WAV"
Extract non-empty intervals: 1, "no"
To Spectrum: "yes"
LPC smoothing: 5, 50

# extract LPC and write to the file -----------------------------------------------------------------------------
n = numberOfSelected ("Spectrum")
for i to n
	spectrum [i] = selected ("Spectrum", i)
endfor

for i to n
	selectObject: spectrum [i]
	data$ = List: "no", "yes", "no", "no", "no", "yes"
	data$ = replace$ (data$, "'newline$'", "'tab$'"+file$ + "'tab$'" + value$ [i] +"'newline$'", 0)
	fileappend "'textfile$'" 'data$'
endfor
