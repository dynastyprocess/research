RB Clustering with Yards Created
================
Joe Sydlowski

  - [Introduction](#introduction)
  - [Why PCA?](#why-pca)
  - [Exploring the Data](#exploring-the-data)
  - [Principal Component Analysis](#principal-component-analysis)
  - [Results](#results)

## Introduction

Principal Component Analysis (PCA) is an unsupervised machine learning
technique used to group players into clusters with similar
characteristics while avoiding overfitting. I have combined this
technique with some notably predictive metrics such as Graham Barfield’s
*Yards Created* and JJ Zachariason’s *prospect model* to create
tier-groupings of rookie running backs.

## Why PCA?

An unsupervised machine learning model does not have an outcome
variable, which is especially useful in prospecting rookie RBs for two
reasons. Many outcome variables set arbitrary thresholds where a top 24
RB season is considered a success but an RB25 finish is then considered
a failure. By using an unsupervised model we can cluster similar RBs
into similar groups and later see how the clusters performed in a
variety of outcome variables. Secondly, using an unsupervised model
prevents us from over-fitting the model on an outcome variable in our
small sample sizes. For example, if you wanted to predict rushing yards
for rookie RBs using draft capital a supervised machine learning model
might look to undrafted free agent Phillip Lindsay’s 2018 rookie season
and underestimate the importance of being drafted. This over-fitting
could result in high predictions for future UDFA RBs as a result.

## Exploring the Data

I used 6 variables (measured during the player’s final collegiate
season) from the two methodologies linked above to tier the 57 RBs with
Yards Created data over the past 5 years:

  - draft\_pick - Overall pick number in the NFL draft
  - Yards Created Variables
      - yc\_per\_attempt - Yards created per attempt
      - mtf\_per\_attempt - Missed tackles forced per attempt
      - rec\_yard\_per\_pass\_play - Receiving yards per pass play
  - JJ’s Prospect Model
      - rec\_share - Share of team receptions
      - total\_yards\_per\_team\_play - Ratio of total yards divided by
        team plays

![](rb_clusters_post_files/figure-gfm/correlations-1.png)<!-- -->

These 6 variables correlate to RB production per game in the first 4
years of their careers. I interpet the Yards Created variables as a
proxy for RB skill as best we can measure it. This is reinforced by the
fact that yards created per attempt and missed tackles forced per
attempt have the two strongest correlations with draft pick. Reception
share measures a team’s intent to get an RB the ball in the passing game
and has a strong correlation to Graham’s charted receiving yards per
pass play. Finally, yards per team pass attempt measures the player’s
efficiency in that offense. These measures of skill, volume, and
efficiency create a strong foundation to create our tiers.

## Principal Component Analysis

Using the tidymodels package in R, I created 6 principal components from
the 6 predictive metrics. The chart below demonstrates which of the
variables contribute most strongly, either positively or negatively, to
each component. Keep in mind that a smaller draft pick is actually
better so the blue bars are later draft picks, while the red bars are
earlier draft picks. The components PC1-PC6 are not ordered in any way
and will need to be intrepreted on their own.

  - First, compare and contrast PC2 to PC5 which are strongly defined by
    early round picks. Whereas PC2 focuses on weak receiving metrics and
    strong measures of efficiency and yards created, a strong receiving
    share is more important to PC5.
  - PC1 is immediately concerning since it is mostly strongly impacted
    by low values in all of our predictive metrics to go along with late
    draft picks. Similarly PC3 consists of late draft picks and low
    scores for yards created and receiving yards per pass play.
  - Finally, PC4 and PC6 also make for an interesting duo. Draft pick
    does not play an significant role in either composition, but where
    PC4 is defined by a strong yards created and weak missed tackles
    forced, PC2 is defined by a weak yards created and strong missed
    tackles forced.

![](rb_clusters_post_files/figure-gfm/variable_importance-1.png)<!-- -->

Below you can see a snapshot of the predictive metrics and examples from
each of the Principal components. The important variables from each of
the components can be seen here.

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#cttlfnjsip .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cttlfnjsip .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cttlfnjsip .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cttlfnjsip .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cttlfnjsip .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cttlfnjsip .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cttlfnjsip .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cttlfnjsip .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cttlfnjsip .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cttlfnjsip .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cttlfnjsip .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cttlfnjsip .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#cttlfnjsip .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cttlfnjsip .gt_from_md > :first-child {
  margin-top: 0;
}

#cttlfnjsip .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cttlfnjsip .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cttlfnjsip .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#cttlfnjsip .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cttlfnjsip .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#cttlfnjsip .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cttlfnjsip .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cttlfnjsip .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cttlfnjsip .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cttlfnjsip .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cttlfnjsip .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#cttlfnjsip .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cttlfnjsip .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#cttlfnjsip .gt_left {
  text-align: left;
}

#cttlfnjsip .gt_center {
  text-align: center;
}

#cttlfnjsip .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cttlfnjsip .gt_font_normal {
  font-weight: normal;
}

#cttlfnjsip .gt_font_bold {
  font-weight: bold;
}

#cttlfnjsip .gt_font_italic {
  font-style: italic;
}

#cttlfnjsip .gt_super {
  font-size: 65%;
}

#cttlfnjsip .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="cttlfnjsip" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table" style="table-layout: fixed;; width: 100%">

<colgroup>

<col/>

<col/>

<col/>

<col/>

<col/>

<col/>

<col/>

<col/>

<col/>

<col/>

<col/>

<col/>

<col style="width:15%;"/>

</colgroup>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Cluster

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Count

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">

<span class="gt_column_spanner">Outcome Metrics</span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6">

<span class="gt_column_spanner">PCA Metrics</span>

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Example Players

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

PPR PPG

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Rec YPG

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Rush YPG

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

FPOE/ Game

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Draft Pick

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

YC/ Att

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

MTF/ Att

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Rec Yards/ Pass

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Rec Share

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Yards/ Team Play

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

PC5

</td>

<td class="gt_row gt_center" style="background-color: #EFE5F0; color: #000000;">

8

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

16.0

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

28.7

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

67.2

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

0.54

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

33.5

</td>

<td class="gt_row gt_center" style="background-color: #EFE5F0; color: #000000;">

4.61

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

0.40

</td>

<td class="gt_row gt_center" style="background-color: #EFE5F0; color: #000000;">

1.30

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

11.2%

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

1.54

</td>

<td class="gt_row gt_left" style="font-size: small;">

Leonard Fournette, Christian Mccaffrey, Josh Jacobs

</td>

</tr>

<tr>

<td class="gt_row gt_left">

PC2

</td>

<td class="gt_row gt_center" style="background-color: #EFE5F0; color: #000000;">

8

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

12.6

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

13.3

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

67.9

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

1.67

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

40.0

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

5.46

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

0.38

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

0.75

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

4.5%

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

1.87

</td>

<td class="gt_row gt_left" style="font-size: small;">

Ezekiel Elliott, D’Onta Foreman, Nick Chubb

</td>

</tr>

<tr>

<td class="gt_row gt_left">

PC4

</td>

<td class="gt_row gt_center" style="background-color: #CBB1D5; color: #000000;">

7

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

11.7

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

18.2

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

45.2

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

0.60

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

73.0

</td>

<td class="gt_row gt_center" style="background-color: #E1F2DC; color: #000000;">

5.41

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

0.33

</td>

<td class="gt_row gt_center" style="background-color: #E1F2DC; color: #000000;">

2.00

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

11.1%

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

1.61

</td>

<td class="gt_row gt_left" style="font-size: small;">

James Conner, Darrell Henderson, Jonathan Taylor

</td>

</tr>

<tr>

<td class="gt_row gt_left">

PC6

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

6

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

10.6

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

19.6

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

55.6

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

0.10

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

86.0

</td>

<td class="gt_row gt_center" style="background-color: #CBB1D5; color: #000000;">

4.61

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

0.42

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

2.30

</td>

<td class="gt_row gt_center" style="background-color: #D9F0D3; color: #000000;">

11.6%

</td>

<td class="gt_row gt_center" style="background-color: #F7F7F7; color: #000000;">

1.81

</td>

<td class="gt_row gt_left" style="font-size: small;">

Kenneth Dixon, Joe Mixon, Zack Moss

</td>

</tr>

<tr>

<td class="gt_row gt_left">

PC1

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

19

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

7.7

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

11.0

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

33.1

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

−0.26

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

114.0

</td>

<td class="gt_row gt_center" style="background-color: #EFE5F0; color: #000000;">

4.61

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

0.29

</td>

<td class="gt_row gt_center" style="background-color: #CBB1D5; color: #000000;">

0.90

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

7.3%

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

1.31

</td>

<td class="gt_row gt_left" style="font-size: small;">

Wayne Gallman, Kalen Ballage, Mark Walton

</td>

</tr>

<tr>

<td class="gt_row gt_left">

PC3

</td>

<td class="gt_row gt_center" style="background-color: #E1F2DC; color: #000000;">

9

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

6.5

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

11.6

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

21.6

</td>

<td class="gt_row gt_center" style="background-color: #C2A5CF; color: #000000;">

−0.15

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

136.0

</td>

<td class="gt_row gt_center" style="background-color: #9970AB; color: #000000;">

4.43

</td>

<td class="gt_row gt_center" style="background-color: #E7D4E8; color: #000000;">

0.36

</td>

<td class="gt_row gt_center" style="background-color: #EFE5F0; color: #000000;">

1.30

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

13.5%

</td>

<td class="gt_row gt_center" style="background-color: #A6DBA0; color: #000000;">

1.98

</td>

<td class="gt_row gt_left" style="font-size: small;">

Jeremy McNichols, Rashaad Penny, Eno Benjamin

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

<!-- # ```{r, echo = FALSE, message = FALSE, warning = FALSE} -->

<!-- # rb_clusters_wide %>% -->

<!-- #   select(name, Cluster, draft_pick, yc_per_attempt, mtf_per_attempt, rec_yard_per_pass_play, rec_share, total_yards_per_team_play) %>% -->

<!-- #   pivot_longer(cols = where(is.numeric), names_to = "Metric") %>% -->

<!-- #   mutate(Metric = factor(Metric, levels = c('draft_pick', 'yc_per_attempt', 'mtf_per_attempt', 'rec_yard_per_pass_play', -->

<!-- #                                             "rec_share", "total_yards_per_team_play"))) %>% -->

<!-- #   ggplot(aes(value, Cluster, fill = 100*stat(ecdf))) +  -->

<!-- #   stat_density_ridges(geom = "density_ridges_gradient", -->

<!-- #                       calc_ecdf = TRUE, -->

<!-- #                       quantiles = 4, -->

<!-- #                       quantile_lines = TRUE, -->

<!-- #                       jittered_points = TRUE, -->

<!-- #                       position = "points_sina", -->

<!-- #                       alpha = 0.8, -->

<!-- #                       point_color = "black" -->

<!-- #   ) +  -->

<!-- #   labs(title = "Distribution of Predictive Variables by Tier", -->

<!-- #        y = NULL, -->

<!-- #        fill = "Percentile") + -->

<!-- #   facet_wrap(~Metric, -->

<!-- #              scales = "free_x", -->

<!-- #              nrow = 3, -->

<!-- #              labeller = as_labeller(c('draft_pick' = 'Overall Draft Pick', -->

<!-- #                                       'yc_per_attempt' = 'Yards Created per attempt', -->

<!-- #                                       'mtf_per_attempt' = 'Missed Tackles Forced per attempt', -->

<!-- #                                       'rec_yard_per_pass_play' = 'Receiving Yards per pass play', -->

<!-- #                                       'rec_share' = 'Reception Share', -->

<!-- #                                       'total_yards_per_team_play' = 'Total Yards per team play'))) -->

<!-- # ``` -->

## Results

Now the fun part: let’s see how these clusters performed in the NFL
based on several outcome varibles. The data points represent one season,
so a players in the 2016 draft class will have 4 points compared to one
point for the class of 2019. While this biases the distributions towards
the players with more data I believe it provides a better view on the
types of seasons we can expect from each tier. Fantasy points over
expectation is based on my own expected points model that will be up on
the site before the start of the season.

  - In a comparison of our early draft pick tiers, PC2 and PC5, the
    strong college reception share of PC5 translates better to NFL
    receiving production. This can be seen with a higher range of
    outcomes in receiving ypg as well as a higher floor for PPR ppg. PC2
    is still my choice for the second strongest of the 6 tiers and
    actually boasts the best outcomes for outperforming their expected
    points.
  - For the reasons outlined above, PC1 and PC3 have seen the worst NFL
    outcomes over the past 4 seasons. OVer 75% of the seasons from these
    players have produced fewer than 10 PPR ppg. Furthermore, they
    consistently produce fewer fantasy points than we would expect given
    their opportunity. While this may be caused by the fact that these
    are lower draft picks than the other tiers, it is also a red flag
    for any higher drafted player that may fall in these tiers.
  - Our final two tiers, PC2 and PC6, show such similar outcomes that I
    cannot say with any confidence that one is preferable to the other.
    Slightly stronger outcomes in PPR ppg, fantasy points over
    expectation, and games played favors PC4, however more data is
    required to draw any conclusions. Both tiers clearly beat out PC1
    and PC3 in most metrics.

![](rb_clusters_post_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
