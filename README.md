# crabpack

This R package generates the standard design-based indices of catch per
unit effort, abundance, and biomass from NOAA Alaska Fisheries Science
Center RACE Eastern Bering Sea Shelf (from 1975) and Northern Bering Sea
Shelf (from 2010) bottom trawl survey data.

*This package and documentation is still under development, please submit 
an issue if you come across a bug.*  
 

<!-- make package logo!! https://github.com/GuangchuangYu/hexSticker -->

## Installation 
```         
devtools::install_github("AFSC-Shellfish-Assessment-Program/crabpack")
```

## Data Access  
There are no legal restrictions on access to the data. They reside in
public domain and can be freely distributed. 

**User Constraints:** Users must read and fully comprehend the metadata
prior to use. Data should not be used beyond the limits of the source
scale. Acknowledgement of AFSC Shellfish Assessment Program, as the
source from which these data were obtained, in any publications and/or
other representations of these data, is suggested.

**General questions and more specific data requests** can be sent to
<shannon.hennessey@noaa.gov>. The version of this data used for stock 
assessments can be found through the Alaska Fisheries Information Network
(AKFIN). <!-- Something about AKFIN DB, who to contact/how to get access... -->

<!-- For questions about the eastern Bering Sea surveys, contact Duane 
Stevenson (<duane.stevenson@noaa.gov>). For questions specifically about 
crab data, contact Mike Litzow (<mike.litzow@noaa.gov>), the Shellfish 
Assessment Program lead.  -->

<!--&nbsp;   
## Collaborators   
The crabpack R package is a product of a AFSC-RACE-SAP working group 
regarding SAP data processes and index computation. Many thanks to those who 
participated in those working groups.

**AKCNOWLEDGE Em/GAP CONTRIBUTION!!**
Matt/AKFIN folks
Shannon Hennessey*  *package maintainer
Emily Ryznar - overall workflow, testing
Erin Fedewa - overall workflow, testing
Jon Richar - sql scrpits/oracle db maintenance
Mike? big picture?

**Data Processes Working Group**|**Index Computation Working Group**|**Supervisors**
:-----:|:-----:|:-----:
Alexandra Dowlin (AlexandraDowlin-NOAA)|Zack Oyafuso (zoyafuso-NOAA)*|Stan Kotwicki (StanKotwicki-NOAA)
Emily Markowitz (EmilyMarkowitz-NOAA)|Margaret Siple (MargaretSiple-NOAA)|Duane Stevenson (Duane-Stevenson-NOAA)
Liz Dawson (liz-dawson-NOAA)|Rebecca Haehn (RebeccaHaehn-NOAA)|Ned Laman (Ned-Laman-NOAA)
Sarah Friedman (SarahFriedman-NOAA)|Lukas DeFilippo (Lukas-DeFilippo-NOAA)|Susanne McDermott (smcdermo) 
Christopher Anderson (ChrisAnderson-NOAA)|Paul von Szalay (vszalay)| 
Nancy Roberson (NancyRoberson)|Thaddaeus Buser (ThaddaeusBuser-NOAA)| 
 |*package maintainer| 

&nbsp;  
## Legacy  
Here is an non-exhaustive list of people who provided the foundation for many 
of the functions in this package:

AI-GOA: Michael Martin, Peter Munro, Ned Laman

Bering Sea: REM, Jason Conner, Jerry Hoff, Rebecca Haehn 

Many of the index calculations are from Wakabayashi et al. (1985):

Wakabayashi, K., R. G. Bakkala, and M. S. Alton. 1985. Methods of the 
     U.S.-Japan demersal trawl surveys, p. 7-29. In R. G. Bakkala and K. 
     Wakabayashi (editors), Results of cooperative U.S.-Japan groundfish 
     investigations in the Bering Sea during May-August 1979. Int. North Pac. 
     Fish. Comm. Bull. 44.
-->



## Relevant Publications  
Zacher, L.S., Hennessey, S.M., Richar, J.I., Fedewa, E.J., Ryznar, E.R., and Litzow,
M.A. (2024). *The 2024 eastern Bering Sea continental shelf trawl
survey: Results for commercial crab species*. [NOAA Tech Memo.](https://repository.library.noaa.gov/view/noaa/66166/noaa_66166_DS1.pdf)


## Organization Acronymns  
**NOAA:** National Oceanic and Atmospheric Administration  
**NMFS:** National Marine Fisheries Service  
**AFSC:** Alaska Fisheries Science Center  
**RACE:** Resource Assessment and Conservation Engineering Division  
**SAP:** Shellfish Assessment Program  


## NOAA Legal Disclaimer  
This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.


## NOAA License  
Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img  align="right" src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>


[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
