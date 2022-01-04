# Sample Locations
The original (1991) sampling locations were selected by an expert group by
placing dots on a map of Casco Bay to get sample locations distributed evenly in
areas known to not have rocky substrate (and where, therefore, soft sediment
samples could be collected).  Originally, each location was given a location
code, of the form "XX##", where "XX" is a two letter region code, and "##" is a
two digit sequence number.

An effort was made in subsequent years to sample the same locations, but for a
variety of reasons, samples were not always collected in quite the same
locations. In the Early 2010s, CBEP staff realized sample locations were
recorded inconsistently, and made a concerted effort to identify where each
sample had actually been collected, often by going back to paper field data
sheets associated with sampling events. 

A large fraction, but not all, observations were collected within a
hundred meters or so of nominal original (1991) sample locations. We grouped
sample locations into Substations (clusters of samples within about 100m of the
original sampling locations), assigning new Substations Codes as needed.  Within
Substations we assigned Locations (within the resolution of the GPS equipment
used at the time, typically ~ 5 to 10 meters, which is smaller than expected
drift during sample collection.).

To simplify data analysis and visualization, samples are "binned" into five
major subregions of the Bay, as follows

(descriptions from 2017 Ramboll Environ  report to CBEP):

> Inner Bay: The Inner Bay includes the western most part of Casco Bay,
encompassing the Fore River and Presumpscot River estuaries. The cities of
Portland and South Portland and the towns of Falmouth and Yarmouth are located
within its drainage.

> Outer Bay: The Outer Bay includes the large open water area to the south of
the other regions. It represents the area that connects Casco Bay to the rest of
the Gulf of Maine.

> West Bay: West Bay extends from Yarmouth on the west to Orrs and Bailey
Islands on the east. It includes Maquoit and Middle Bays, Harpswell Sound, and
the communities of Freeport, Harpswell, and parts of southern Brunswick. The
Royal and Harraseeket Rivers discharge into West Bay.

> East Bay: East Bay includes the inland portions of Casco Bay bordered by Orrs
and Bailey Islands on the west and Phippsburg on the east. It includes Quahog
Bay and the New Meadows River in southern Brunswick and Phippsburg.

> Cape Small: Cape Small is the easternmost region of the bay. It includes the
southern end of the Phippsburg peninsula to Small Point. The mouth of the Lower
Kennebec River flows into the Gulf of Maine to the east of Phippsburg. While it
is not part of Casco Bay, coastal circulation patterns indicate that the
discharge from the Lower Kennebec (which includes flows from the Kennebec and
Androscoggin Rivers) is entrained into the bay in the Cape Small area (Janzen et
al. 2005).

Locations are in decimal degrees and UTM coordinates, based on WGS 1984.

Places where samples were collected were presented as:

1.  Stations -- nominal sampling locations consistently used for sample planning
    across eras. For a variety of reasons, including transcription errors,
    varying sampling plans over more than 20 years, and inability to collect
    samples at the nominal location, distances between samples associated with a
    single Station can sometimes be substantial. Stations are not used in these 
    analysis becaus of those inconsistencies.
    
2.  Substation -- Nominal Locations associated with stations. Substations
    aggregate multiple sampling events into groups by proximity. All
    samples assigned to the same Substation were collected within a radius of
    approximately 100 m.

3.  Locations -- These provide more detailed estimates of location, with all
    observations associated with a single location collected within a distance 
    of approximately 10 meters of one another.

### `locations.csv`
### `substations.csv`

These two files contain the same data columns, as follows:

Column Name  | Contents                                                      
-------------|--------------------------------------------------------
Location     | Location code
Substation   | Substation Code  (Multiple Locations exist for some substations)
Lat_N        | Latitude, WGS 1984, in decimal degrees
Long_W       | Longitude, WGS 1984, in decimal degrees
UTM_Easting  | UTM Coordinates, Zone 19 North, WGS 1984
UTM_Northing | UTM Coordinates, Zone 19 North, WGS 1984
Location Comment | Narrative comment associated with the LOCATION
Region       | Which region of hte Bay is this location in

The two files overlap substantially in content.  The primary difference is that
`locations.csv` includes all locations, while `substations.csv` includes all
substations (and thus includes only those locations used to define the location 
of the substation).

#  Shapefiles
Shapefiles were produced as follows:

1.  Data from the 'locations.csv' and 'substations.csv'  were imported into 
    ArcGIS, as tables.

2.  We generated event layers from those data tables (based on UTM coordinates,
    Zone 19 N, NAD 1983), and

3.  Exported the resulting layers as shapefiles.
