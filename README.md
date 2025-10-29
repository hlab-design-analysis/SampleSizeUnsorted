# SampleSizeUnsorted
Determination of sample size needed to attain specific margins of error in the species composition of unsorted landings.

**Methodology**
------------
Sample sizes needed to attain pre-specified margins of error are calculated based on sampling theory (Särndal et al., 1992; Lohr, 2010). Full documentation can be found [**here (eng)**](https://github.com/hlab-design-analysis/SampleSizeUnsorted/blob/24b47feda353ea3aedf8f8241338983e3b6822c8/documentation/Determination%20of%20sample%20size.docx). An Example of its application to data collected from a Swedish fishery can be found [**here**](https://github.com/hlab-design-analysis/SampleSizeUnsorted/blob/7a161e05242b6de9a543298ce3422319aba4e4c3/documentation/Weighing%20Project%20Report_AnnexG1.pdf). Examples of its application to Finish and Irish control data can be found [**here**](https://github.com/hlab-design-analysis/SampleSizeUnsorted/blob/7a161e05242b6de9a543298ce3422319aba4e4c3/documentation/Weighing%20Project%20Report_AnnexG5.pdf).

Code is implemented as an R script. See in script [estimSppComp.r](https://github.com/hlab-design-analysis/SampleSizeUnsorted/blob/dev/estimSppComp.r) how to apply this procedure to your data

**Main references:**

Lohr, S. (2010). Sampling: Design and Analysis, 2nd edition. Brooks/Cole.

Särndal, C.-E., Swensson, B. och Wretman, J. (1992). Model Assisted Survey Sampling. Springer-Verlag.

**How to install/download**
------------
Click the green button "Code" on the top of the page and select the option you are more familiar with.

**Input data**
------------
The input data to the script is a data.frame. The following columns should be present:
- **_lanID_**: a unique code that identifies the landing event. Should be the same in all buckets taken from that landing.
- **_bucID_**: a unique code that identifies the landing. Should be the same in all species observed in that landing.
- **_sp_**: 3-letter FAO code of the species.
- **_sppWeight_obs_**: weight of species _sp_ found inside the basket in kg.
- **_totWeight_obs_**: total weight of the landing event in kg.

A few consistency checks on input data are run in the beginning of the script (function `doInitialChecks`).

**Contributors**
------------
(cre, aut) Nuno Prista. Swedish University of Agricultural Sciences.

(aut) Annica de Groote. Swedish University of Agricultural Sciences. 


