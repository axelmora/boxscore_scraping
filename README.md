# boxscore_scraping
A set of R functions to make web scraping in a single MLB box score XML file.
The master box function is integrated by another individual functions, each function provides the following data of the game:

* DATE date of the game
* AWAY Away team
* aR Away Runs
* aH Away Hits
* aE Away Errors
* AW Away Wins
* AL Away Losses
* HOME Home team
* hR Home runs
* hH Home hits
* hE Home errors
* HW Home wins
* HL Home losses
* ATT Game attendance
* TIME Game duration in minutes

The box function collect the previous data and create a data frame with this information, as this:

  DATE        AWAY aR aH aE AW AL HOME hR hH hE HW HL  ATT TIME <br>

1 2017-06-15  OAX  2  9  1 28 35  PUE 11 18  0 32 33 2777  181

A example for a box score XML file is this:
http://gd.mlb.com/components/game/aaa/year_2017/month_06/day_15/gid_2017_06_15_oaxaaa_pueaaa_1/rawboxscore.xml

Is a Mexican League game (aaa).
