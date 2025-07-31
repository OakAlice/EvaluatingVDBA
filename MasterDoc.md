###### Invite for Paper
- 26/07: Chris was invited to write a paper on the value and challenges of VDBA for accelerometry. The following is the invite:
	> "In field studies on freely moving animals neither direct nor indirect calorimetry can be applied. Instead accelerometery or heart rate are used to predict metabolic rate, daily energy expenditure, or the energetics of movements.  You have done a lot of accelerometry/locomotion work which is essential for the understanding and translation into animal energetics. I recently realized how difficult it is to translate physical movement records into energetics when measuring the locomotor activity of Djungarian hamsters with implanted transmitters (changes of electromagnetic field) in parallel with metabolic rate and changes in surface temperature via IR-thermography (Heldmaier et al. 2024 Fig. 11 attached). ==Are simple correlations okay or do we need more?
	> It would be great if you could contribute a manuscript on the use of accelerometry and related methods for the analysis of animal energetics to the Special Isssue on the Fire of Life.=="
###### Chris' Idea
- Original idea he came up with was to look at scaling. Following is a quote from the email he sent:
	- **Title:** How are changes in activity reflected in changes in energy use across taxonomic and body mass scales
	- **Idea:** We have collated a broad sample of accelerometer data from a range of species (~30-40 species) which vary in taxa and body mass. From this we can devise three simple variables:
		 1 – Overall activity
		 2 – Activity intensity during periods of active movement (requires a threshold)
		 3 - Activity intensity during periods of rest (requires a threshold)
	 We would then correlate these with three measures of metabolic rate from the literature:
		 1 – Field metabolic rate (measure using isotope decay)
		 2- Basal metabolic rate
		 3- Maximal metabolic rate
	 The output would be an understanding of the broad patterns between these variables. Perhaps we could devise a metric for predicted activity for a body mass, then we can estimate if certain species are more / less active than predicted. Secondly I would be interested in estimating how energy use varies with body size, among different taxa / locomotory modes / habitats. How does it scale, and to what extent might this predict changes in energy use?
- While the journal editor accepted this proposal, I feel uncertain about it and can't quite accept the idea fully without doing some additional research... I'm not certain of my reluctance, but I think it's because I either think it's already been done, or that the question isn't quite valid... but I think this is only because I haven't read any of the metabolic literature myself.
###### My Idea
- My idea is based on a conversation I had with Pravna Minasandra at the MPI the other week. He expressed frustration that if you put 4 accelerometers on 4 different parts of a bike, an ecologist would draw the false conclusion that each part of the bike did different amounts of energy... which is invalid -- point being that diffrences in collar placement make VDBA an invalid comparison:
	- Using the bio-physics simulations of animal locomotion developed in OpenSim, simulate an accelerometer point on various parts of the body, calculate VDBA for energetically identical events (which we know based on the energetics model in the OpenSim), and see how the following variables affect:
		- Collar placement
		- Collar looseness
		- Animal size 
		- Animal weight
		- Animal morphology
	- I hypothesis that there will be a threshold at which the VDBA becomes significantly different for each of these variables, even when the underlying event is energetically the same. To be more precise:
		- Given the same energetic output (as derived within the models), accelerometer-derived VDBA will vary within species, depending on tag placement and looseness body size and weight, and across species, depending on morphology.
	- This will show that cross-species and cross-individual differences in VDBA might be spurious, and not actually reflect underlying energetic differences.
- Relevant papers:
	- https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2656.13040?utm_source=chatgpt.com (3rd paper in the list below btw)
		- Mentions each of the collar variables but doesn't quantify it in any way...



#### Reading List
- Halsey, L.G., Shepard, E.L.C., Wilson, R.P. (2011). "Assessing the development and application of the accelerometry technique for estimating energy expenditure." Comparative Biochemistry and Physiology Part A.
- Gleiss, A.C., Wilson, R.P., Shepard, E.L.C. (2011). "Making overall dynamic body acceleration work: on the theory of acceleration as a proxy for energy expenditure." Methods in Ecology and Evolution.
- Wilson, R.P. et al. (2020). "Estimates for energy expenditure in free-living animals using acceleration proxies: a reappraisal." J Anim Ecol.