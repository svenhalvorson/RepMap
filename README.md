Hi all,

I was interested in looking at the proportional representation in congress so I created [this map](https://public.tableau.com/views/CongressionalPowerMap/CongressionalPowerMap?:embed=y&:display_count=yes&publish=yes&:toolbar=no). I want to add some details here as my
method might not be intuitive for everyone.

**Example calculations**

1. Representation ratio for Texas in the 1900 House of representatives: 
 + Texas population = 3,048,710
 + Represented US population = 74,607,225
 + Texas seats = 16
 + Total seats = 386
 + Representation ratio = (16/386)/(3,048,710/74,607,225) = 1.014

2. Utah's relative house power to Texas in 1990 (select Texas on left, hover over Utah)
 + Texas population = 3,048,710
 + Texas seats = 16
 + Utah population = 276,749
 + Utah seats = 1
 + Relative power = (3,048,710/16)/(276,749/1) = 0.73

The reason I phrase it as 'Utah's power relative to Texas' but put Utah in the denominator is because
having a smaller ratio of people to representatives means more power per citizen.

**Data Wrangling**

The data comes from [here](https://en.wikipedia.org/wiki/List_of_U.S._states_by_historical_population) and [here](https://en.wikipedia.org/wiki/United_States_congressional_apportionment)

I made a number of choices while collecting the data that should be mentioned:

1. The total US population comes from the total of all of the states that were in the union at that time. DC was not included.
2. The years represent the census years and the representative counts are those apportioned three years later.
3. Census data for West Virginia was collected while it was part of Virginia. I added them together and labeled it as Virginia for those years.
4. The same is true of Maine and Massachusetts.
