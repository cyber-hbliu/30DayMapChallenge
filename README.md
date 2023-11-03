# 30DayMapChallenge

## #1: Points - Popular Instagram Check-ins in North America

I mapped the popular check-in points under the hashtag "ustrip" in North America on Instagram. The data is from [Voratham Tiabrat](https://zenodo.org/records/3530864#.Y9Y5itJBwUE), including the records during Jan 2016 to Oct 2019. I mapped it with R and modified it in Adobe Illustrator. 

Packages that I used include `ggplot`, `rnaturalearth`, `sf` and `dplyr`.

![map](https://github.com/cyber-hbliu/30DayMapChallenge/blob/1f28f8f404d33ab921691dc18219d583cce79363/1/map.png)



## #2: Lines - Motor Vehicle Collisions Index Per Street in Manhattan

I mapped the change of the MVC Index per street in Manhattan from January to October of the year 2023. The data is from [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95), containing details on the crash event from all police-reported motor vehicle collisions in NYC. The MVC Index provides a measure of crash frequency per street. It was calculated based on street network length, nearest edge, and the number of crashes, then logged and normalized.

Packages that I used include `OSMnx` to get street network graphs and graph edges, `matplotlib` to plot the histogram and maps, `folium` to visualize the interactive map.

![map](https://github.com/cyber-hbliu/30DayMapChallenge/blob/260b059e83e6f1ac74e942f3724594e099f550cc/2/1.gif)
