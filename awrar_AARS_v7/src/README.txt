
Changes from version 6 to 7
- Update AARS to subtract the river area from the the alluvium area
- only allow dead storage evap and rainfall when dead storage is not full
- For above, moved dead storage to before river evap and rainfall, this is a big change as it now becomes before anabranches
- fixed bug with dead storages - they were not taking into account the time step length
Other notes: some sensitivity is found for small changes when crossing thresholds - this can cause larger than expected differences in results for small changes in input 


Changes from version 5 to 6
- combined DMAARS, AARS and DS models
