This is the UI using Shiny of the wikipedia.de that was used in the PPTAM paper.

8/7/2020  'ShinyPPTAMtabsWorking9.R' is the calling program.  'PolygonSource_working2.R' is the subroutine.  The data is in the PPTAM_EXT folder (which is in the other folder in this Github account).  This program computes the numbers for the polygon and I'm using 'wikipedia-profile-de.csv' for the user requests.  This is a workaround in order to compute the scale factor.
9/12/2020  'app.R' is 'ShinyPPTAMtabsWorking9.R' and 'PolygonSource_working2.R' put together and is the file that is deployed on the web at this time.  It has the 3 domain metrics: Mem, CPU, and CartReplicas.

9/23/2020  Created v2 folder with changes to the PPTAM UI with a drop-down box and a more user-friendly grid on the third tab and made the code much easier to adapt to new set-ups.

9/30/2020  Do not use v2 folder because that version takes too long to load up.  Use the files in the main "UIShiny_files" folder.
