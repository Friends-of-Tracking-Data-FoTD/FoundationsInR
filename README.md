# FoundationsInR
This repository will contain getting started material with R. It will try to cover the topics of how to begin programming with R: 

-Data transformation (working with scalars, arrays, vectors, lists, dataframes, matrices, data tables)<br/>
-Data visualization<br/>
-Performing analysis<br/> 
-Interactive applications in Shiny<br/>

The code that is available at the moment is the following:
* Draw_Pitch.R
  * A file that draws the pitch used for visualizing data on a soccer field. It hasn't yet be turned into a function, but the first two  lines of code will let you set the pitch dimensions. It is set to 120 x 80 based on the dimensions used by StatsBomb. 
  * Learn to create your own pitch from scratch with FC_rStats (https://github.com/FCrSTATS/Visualisations/blob/master/3.CreateAPitch.md)
  
* Statsbomb.R
  * A file containing how to get started with the StatsBomb open data, provided to us graciously via this link: https://github.com/statsbomb/open-data. 
  
* Statsbomb_Shiny_Part1.R
  * Most of the code is reused from Statsbomb.R except we now create a basic shiny application to visualize shot map information

* Statsbomb_Shiny_Part2.R
  * An extension from Part 1 to build a fully functional interactive shot map via R Shiny. Goes more in-depth into the UI and Server pieces within an application. 

Some additional repos that may help: 
  * https://github.com/statsbomb/StatsBombR (Feel free to look at the code for how they pull data to see the difference between what is provided in this repo vs other ways to transform data)
  * https://github.com/etmckinley/PassSonar
  * https://ryo-n7.github.io/2019-08-21-visualize-soccer-statsbomb-part-1/
  * https://github.com/etmckinley/Action-Density
  * https://github.com/etmckinley/Freeze-Frame
