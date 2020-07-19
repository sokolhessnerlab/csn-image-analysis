Clean Qualtrics Export
======================

Load packages
-------------

``` r
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(hash)
```

Constants
---------

``` r
QUALTRICS_FILENAME = "qualtrics.tsv"
```

Set datapath and load `shlab.imgct`
-----------------------------------

Begin by setting the working directory and important top-level paths to
data and loading necessary packages.

-   NOTE: This will be changed to dynamically account for the package
    `shlab.imgct` via its GitHub instance later. For now, it is using
    development loading.

``` r
# Set the working directory to be part of S Drive (may make dynamic later?)
# Whilst not dynamic, change for own session if mount point is not equivalent on
# local machine
shared_dir <- "~/Projects/shlab/mounts/imgct"
package_dir <- "~/Projects/shlab"

datapath <- file.path(shared_dir, "csn_images")
imgct_package_path <- file.path(package_dir, "shlab.imgct")

# Make sure that devtools, tidyverse are installed before this call
devtools::load_all(imgct_package_path)
```

Load Qualtrics TSV
------------------

Using the convience method `shlab.imgct::load_qualtrics_tsv` will load a
TSV export of Qualtrics response data collected from the image
categorization task. (Please note that the output of this raw dataset is
hidden to maintain participant privacy.)

``` r
qualtrics_export <- shlab.imgct::load_qualtrics_tsv(datapath)
```

Parse Qualtrics Export
----------------------

Remove unnecessary columns of data from the Qualtrics exported data, and
remove participant rows in which the task was not complete.

``` r
qualtrics_export_parsed <- shlab.imgct::parse_qualtrics_export(qualtrics_export)

head(
  qualtrics_export_parsed
)
```

    ##   participant_id imageBlock X1_Q10 X2_Q10 X3_Q10 X4_Q10 X5_Q10 X6_Q10 X7_Q10
    ## 1        ICT_001         01      1      5      4      4      1      3      1
    ## 2        ICT_002         01      1      3      3      3      3      3      1
    ## 3        ICT_003         01      1      5      4      3      1      3      1
    ## 4        ICT_004         01      1      3      3      3      1      3      1
    ## 5        ICT_005         01      1      3      4      3      1      3      1
    ## 6        ICT_006         01      1      3      3      3      1      3      1
    ##   X8_Q10 X9_Q10 X10_Q10 X11_Q10 X12_Q10 X13_Q10 X14_Q10 X15_Q10 X16_Q10 X17_Q10
    ## 1      1      1       1       3       5       1       1       2       1       1
    ## 2      1      1       1       4       3       1       1       1       1       1
    ## 3      1      1       1       3       3       1       1       2       1       1
    ## 4      1      1       1       3       4       1       1       2       1       1
    ## 5      1      1       1       3       4       1       1       2       1       1
    ## 6      1      1       1       4       4       1       1       2       1       1
    ##   X18_Q10 X19_Q10 X20_Q10 X21_Q10 X22_Q10 X23_Q10 X24_Q10 X25_Q10 X26_Q10
    ## 1       4       1       2       1       1       2       1       1       1
    ## 2       4       4       4       2       1       5       1       1       1
    ## 3       4       1       2       1       1       2       1       1       1
    ## 4       4       1       2       1       1       3       1       1       1
    ## 5       3       1       2       1       1       3       1       1       1
    ## 6       5       1       2       1       1       4       1       1       1
    ##   X27_Q10 X28_Q10 X29_Q10 X30_Q10 X31_Q10 X32_Q10 X33_Q10 X34_Q10 X35_Q10
    ## 1       2       1       2       2       1       3       3       3       2
    ## 2       2       2       2       2       3       3       3       1       2
    ## 3       2       1       2       2       1       3       3       1       2
    ## 4       2       1       2       2       1       3       4       3       2
    ## 5       2       1       2       2       1       4       3       1       2
    ## 6       2       1       2       2       1       3       1       1       2
    ##   X36_Q10 X37_Q10 X38_Q10 X39_Q10 X40_Q10 X41_Q10 X42_Q10 X43_Q10 X44_Q10
    ## 1       3       2       1       3       1       1       1       2       3
    ## 2       3       2       1       3       1       1       1       1       1
    ## 3       3       2       1       3       1       1       1       1       3
    ## 4       3       2       1       3       1       1       1       1       3
    ## 5       3       2       1       3       1       1       1       1       3
    ## 6       4       2       1       3       1       1       1       1       3
    ##   X45_Q10 X46_Q10 X47_Q10 X48_Q10 X49_Q10 X50_Q10 X51_Q10 X52_Q10 X53_Q10
    ## 1       4       2       3       5       3       1       1       1       1
    ## 2       3       2       4       4       4       1       1       1       1
    ## 3       4       2       4       4       3       1       1       1       1
    ## 4       3       2       3       4       3       1       1       1       1
    ## 5       3       2       3       4       3       1       1       1       1
    ## 6       4       2       4       4       4       1       1       1       1
    ##   X54_Q10 X55_Q10 X56_Q10 X57_Q10 X58_Q10 X59_Q10 X60_Q10 X61_Q10 X62_Q10
    ## 1       1       1       1       4       4       4       2       4       3
    ## 2       1       1       1       1       2       1       2       4       4
    ## 3       1       1       1       1       2       1       2       4       4
    ## 4       1       1       1       1       2       1       2       4       3
    ## 5       1       1       1       1       4       1       2       4       4
    ## 6       1       1       1       1       2       1       2       4       4
    ##   X63_Q10 X64_Q10 X65_Q10 X66_Q10 X67_Q10 X68_Q10 X69_Q10 X70_Q10 X71_Q10
    ## 1       4       1       5       5       2       2       1       3       3
    ## 2       4       1       4       2       2       1       1       4       4
    ## 3       4       1       4       4       2       2       1       4       3
    ## 4       4       1       3       2       2       2       1       4       3
    ## 5       4       1       4       4       2       2       1       3       3
    ## 6       4       1       4       4       2       2       1       4       4
    ##   X72_Q10 X73_Q10 X74_Q10 X75_Q10 X76_Q10 X77_Q10 X78_Q10 X79_Q10 X80_Q10
    ## 1       3       1       4       1       1       2       3       1       3
    ## 2       3       1       4       1       1       2       3       1       3
    ## 3       3       1       4       1       1       2       3       1       3
    ## 4       3       1       4       1       1       2       3       1       3
    ## 5       3       1       4       1       1       2       3       1       3
    ## 6       3       4       4       1       1       2       3       1       3
    ##   X81_Q10 X82_Q10 X83_Q10 X84_Q10 X85_Q10 X86_Q10 X87_Q10 X88_Q10 X89_Q10
    ## 1       4       3       1       1       3       2       4       1       4
    ## 2       1       3       1       1       1       2       4       1       1
    ## 3       1       3       1       1       1       2       4       1       1
    ## 4       1       3       1       1       3       2       4       1       1
    ## 5       1       3       1       1       3       2       4       1       1
    ## 6       1       3       1       1       1       2       4       1       1
    ##   X90_Q10 X91_Q10 X92_Q10 X93_Q10 X94_Q10 X95_Q10 X96_Q10 X97_Q10 X98_Q10
    ## 1       4       4       1       3       2       5       1       1       3
    ## 2       4       4       1       4       3       4       1       1       4
    ## 3       3       4               4       2       4       1       1       4
    ## 4       3       4       1       4       2       4       1       1       4
    ## 5       4       4       1       3       3       4       1       1       4
    ## 6       4       4       1       4       3       4       1       1       4
    ##   X99_Q10 X100_Q10 X101_Q10 X102_Q10 X103_Q10 X104_Q10 X105_Q10 X106_Q10
    ## 1       1        4        1        1        1        1        3        4
    ## 2       1        4        1        1        1        1        1        4
    ## 3       1        4        1        1        1        1        3        4
    ## 4       1        4        1        1        1        1        3        4
    ## 5       1        4        1        1        1        1        1        4
    ## 6       1        4        1        1        1        1        1        4
    ##   X107_Q10 X108_Q10 X109_Q10 X110_Q10 X111_Q10 X112_Q10 X113_Q10 X114_Q10
    ## 1        3        1        2        2        1        4        3        1
    ## 2        3        2        2        2        1        1        3        1
    ## 3        3        2        2        2        1        1        3        1
    ## 4        3        2        2        2        1        4        3        1
    ## 5        3        2        2        2        1        1        3        4
    ## 6        3        1        2        2        1        4        3        1
    ##   X115_Q10 X116_Q10 X117_Q10 X118_Q10 X119_Q10 X120_Q10 X121_Q10 X122_Q10
    ## 1        1        5        3        2        1        3        1        4
    ## 2        1        3        3        2        1        3        1        1
    ## 3        1        1        3        2        1        3        1        1
    ## 4        1        1        3        2        1        3        1        1
    ## 5        1        4        3        2        1        3        1        4
    ## 6        1        1        1        2        1        3        1        1
    ##   X123_Q10 X124_Q10 X125_Q10 X126_Q10 X127_Q10 X128_Q10 X129_Q10 X130_Q10
    ## 1        3        3        1        1        1        4        1        4
    ## 2        1        4        1        1        1        3        1        1
    ## 3        1        4        1        1        1        2        1        4
    ## 4        3        4        1        1        1        3        1        3
    ## 5        3        4        1        1        1        4        1        4
    ## 6        3        4        1        1        1        4        1        1
    ##   X131_Q10 X132_Q10 X133_Q10 X134_Q10 X135_Q10 X136_Q10 X137_Q10 X138_Q10
    ## 1        3        4        1        1        4        1        1        4
    ## 2        4        4        1        1        4        1        1        1
    ## 3        4        1        1        1        4        1        1        1
    ## 4        4        1        1        1        4        1        1        1
    ## 5        4        4        1        1        4        3        1        4
    ## 6        4        1        1        1        4        1        1        1
    ##   X139_Q10 X140_Q10 X141_Q10 X142_Q10 X143_Q10 X144_Q10 X145_Q10 X146_Q10
    ## 1        4        3        1        3        2        2        3        1
    ## 2        1        1        1        1        2        2        1        1
    ## 3        1        1        1        1        2        2        3        1
    ## 4        1        1        1        1        2        2        3        1
    ## 5        1        1        1        1        2        2        1        1
    ## 6        1        1        1        1        2        2        2        1
    ##   X147_Q10 X148_Q10 X149_Q10 X150_Q10 X151_Q10 X152_Q10 X153_Q10 X154_Q10
    ## 1        3        2        3        1        1        4        1        3
    ## 2        3        2        1        1        2        1        3        2
    ## 3        3        2        1        1        1        1        1        3
    ## 4        3        2        3        1        1        1        1        3
    ## 5        3        2        1        1        1        4        1        3
    ## 6        3        1        1        1        1        1        1        3
    ##   X155_Q10 X156_Q10 X157_Q10 X158_Q10 X159_Q10 X160_Q10 X161_Q10 X162_Q10
    ## 1        1        1        1        2        3        1        1        4
    ## 2        1        1        1        2        3        1        3        1
    ## 3        1        1        1        3        3        1        1        4
    ## 4        1        1        1        2        3        1        1        4
    ## 5        1        4        1        2        3        1        1        4
    ## 6        1        1        1        2        3        1        1        4
    ##   X163_Q10 X164_Q10 X165_Q10 X166_Q10 X167_Q10 X168_Q10 X169_Q10 X170_Q10
    ## 1        2        3        1        1        1        4        2        1
    ## 2        1        4        3        1        1        1        1        2
    ## 3        2        3        1        1        1        4        2        1
    ## 4        2        3        1        1        1        4        2        1
    ## 5        2        3        1        1        1        4        2        1
    ## 6        2        3        1        1        1        4        2        1
    ##   X171_Q10 X172_Q10 X173_Q10 X174_Q10 X175_Q10 X176_Q10 X177_Q10 X178_Q10
    ## 1        1        1        1        4        1        1        1        4
    ## 2        3        1        4        2        3        4        4        4
    ## 3        1        1        1        4        1        1        1        4
    ## 4        1        1        1        4        1        1        1        4
    ## 5        1        1        1        4        1        1        1        4
    ## 6        1        1        1        4        1        1        1        4
    ##   X179_Q10 X180_Q10 X181_Q10 X182_Q10 X183_Q10 X184_Q10 X185_Q10 X186_Q10
    ## 1        2        3        2        4        4        2        5        1
    ## 2        4        3        2        4        4        2        1        1
    ## 3        3        3        2        4        4        2        1        1
    ## 4        2        3        2        4        4        2        1        1
    ## 5        2        3        2        4        4        2        1        1
    ## 6        2        3        2        4        4        2        1        1
    ##   X187_Q10 X188_Q10 X189_Q10 X190_Q10 X191_Q10 X192_Q10 X193_Q10 X194_Q10
    ## 1        1        5        4        1        4        3        1        1
    ## 2        1        1        1        2        2        4        1        3
    ## 3        1        1        4        1        1        3        1        1
    ## 4        1        1        4        1        1        3        1        1
    ## 5        1        1        4        1        4        3        1        1
    ## 6        1        1        4        1        1        3        1        1
    ##   X195_Q10 X196_Q10 X197_Q10 X198_Q10 X199_Q10 X200_Q10 X201_Q10 X202_Q10
    ## 1        1        1        3        1        5        3        3        2
    ## 2        4        4        3        1        5        2        1        3
    ## 3        1        1        3        1        5        3        3        2
    ## 4        1        3        3        1        5        3        5        2
    ## 5        1        1        3        1        5        4        3        2
    ## 6        1        1        3        1        5        4        4        2
    ##   X203_Q10 X204_Q10 X205_Q10
    ## 1        1        4        4
    ## 2        1        2        3
    ## 3        1        4        4
    ## 4        1        4        4
    ## 5        1        4        4
    ## 6        1        4        4

Clean Qualtrics Export
----------------------

The two above demonstrations are included within the function to clean
Qualtrics exported survey data for this task. Additionally, the `clean`
function is a convenience on top of `clean_qualtrics_export` that
(currently) only allows for Qualtrics TSV response data. Within the
call, TXT files of each image block are loaded to rename columns for
each block of image categorization rating responses, as Qualtrics also
unfortunately replaces columns with each block of images surveyed.

`{, clean-qualtrics-export, eval=False} # Here, we demonstrate the underlying Qualtrics-specific method shlab.imgct::clean_qualtrics_export(datapath, filename = QUALTRICS_FILENAME)`

Using the convenient abstraction `clean`, we can load, parse, and clean
each block of image categorization rating responses across participants.
This will, notably, remove any participant that that has errors in their
responses, too. If successful, the cleaned blocks will be saved to the
`~/datapath/clean` directory.

``` r
shlab.imgct::clean(datapath, filename = QUALTRICS_FILENAME)
```

    ## [1] "Success! Your clean blocks were saved to  ~/Projects/shlab/mounts/imgct/csn_images/clean"
