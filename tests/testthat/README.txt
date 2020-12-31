Here I am creating a list of tests which should be implemented:

General Testing:

- Basic single strata shape no holes simulation - check it runs
- Single strata shape with hole - check it runs
- Complex multi-strata shape - check it runs
- Line transect simulation - check it runs
- Point transect simulation - check it runs
- Segmented line transect simulation - check it runs
- Half-normal detectability - check it runs and produces half-normal shape
- Hazard-rate detectability - check it runs and produces hazard-rate shape
- Uniform detection function - check it runs and produces uniform detection probability

Input testing:

-

Special specific testing:

- Create a test transect set and population to verify calculations in cal.perp.dists() - tests should cover horizontal, vertical and another set of transcts at an angle other than these options.

- Check that dsims correctly re-numbers sightings of the same object detected from 2 different transects. Most extreme test case here would be largely overlapping covered areas and a uniform detection probability of 1.



