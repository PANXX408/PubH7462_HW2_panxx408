PubH7462_hw2_panxx408
================
Mingming Pan
2/9/2022

``` r
#Read data
brfss_df <- read_csv("./data/brfss_smart_2010.csv")
```

## Problem3.1 Data Exploration & Cleaning

``` r
#Data Exploration
plot_str(brfss_df)
plot_intro(brfss_df)
```

<img src="PubH7462_HW2_panxx408_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" style="display: block; margin: auto;" />

``` r
#Date Cleaning
brfss_clean <- brfss_df %>% separate(col = Locationdesc, into = c("State", "2", "County"),
                                     sep = " ") %>%
  filter(Topic == "Overall Health") %>%
  select(Year, State, County, Response, Sample_Size, Data_value) %>%
  rename(Response_Prop = Data_value) %>%
  mutate(State = as.factor(State)) %>%
  mutate(County = as.factor(County))
```

## 3.2 Data Description

-   There are 6 variables and 10625 observations.
-   Each observation represents details of one of the response of a
    county at a specific year.
-   Year, State, County, Response, Response Proportion and sample size
    are the six variables.
-   Year describes the year taking the questionnaire; State and County
    describe the locations; Response describes the type of responses and
    response proportion indicates the proportion of corresponding
    response; and sample size describes the sample size of corresponding
    response.
-   The number of countys taking the questionnaire varies from year to
    year.

## 3.3 Do Data Science

### 3.3.1In the year 2004, which states were observed at 6 locations?

``` r
brfss_clean %>%
  filter(Year == "2004") %>%
  select(State, County) %>%
  distinct() %>%
  group_by(State) %>%
  summarise(Locations = n()) %>%
  filter(Locations == 6) %>%
  gt() %>%
  cols_label(Locations = "Number of Locations") %>%
  tab_header("States were Observed at 6 Locations in 2004")
```

<div id="inrxdjkvtb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>States were Observed at 6 Locations in 2004</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">State</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Number of Locations</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_center">CO</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">NM</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">SC</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">TX</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">UT</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">VT</td>
<td class="gt_row gt_right">6</td></tr>
  </tbody>
  
  
</table>
</div>

**CO, NM, SC, TX, UT and VT were observed at 6 locations in 2004.**

### 3.3.2 Make a spaghetti plot that shows the number of observed locations in each state from 2002 to 2010. Which state has the highest mean number of locations over this period?

``` r
brfss_clean %>% filter(Year > 2001 & Year < 2011) %>%
  select(Year, State, County) %>%
  distinct() %>% 
  group_by(State, Year) %>%
  summarise(Location = n()) %>% 
  ungroup(Year, State) %>%
  mutate(State = fct_reorder(State, Location, mean, .desc = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = Location, group = State, color = State)) +
  labs(
    x = "Year",
    y = "Number of Locations",
    title = "Mean of Locations by State from 2002 - 2010"
  )
```

<img src="PubH7462_HW2_panxx408_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" style="display: block; margin: auto;" />

**NJ is the state which has the highest mean number of locations over
the period.**

### 3.3.3 Make a table showing, for the years 2002, 2006, and 2010, the mean and standard deviation of sample size and proportion of Excellent, Good, and Poor responses across locations in MN.

``` r
brfss_clean %>% filter(Year %in% c("2002", "2006", "2010"),
                       State == "MN",
                       Response %in% c("Excellent", "Good", "Poor")) %>%
  group_by(Response, Year) %>%
  summarise(across(c(Sample_Size, Response_Prop), 
                   list(mean = mean,
                        sd = sd),
                   na.rm = FALSE,
                   .names = "{.col}.{.fn}")) %>%
    gt() %>%
  tab_header("Response Proportion and Sample Size in MN on 2002, 2006 and 2010") 
```

<div id="nnjvlzucdw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Response Proportion and Sample Size in MN on 2002, 2006 and 2010</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Year</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Sample_Size.mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Sample_Size.sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Response_Prop.mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Response_Prop.sd</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">Excellent</td>
    </tr>
    <tr><td class="gt_row gt_right">2002</td>
<td class="gt_row gt_right">116.00</td>
<td class="gt_row gt_right">83.275</td>
<td class="gt_row gt_right">24.15</td>
<td class="gt_row gt_right">3.5407</td></tr>
    <tr><td class="gt_row gt_right">2006</td>
<td class="gt_row gt_right">122.33</td>
<td class="gt_row gt_right">72.625</td>
<td class="gt_row gt_right">23.83</td>
<td class="gt_row gt_right">2.9872</td></tr>
    <tr><td class="gt_row gt_right">2010</td>
<td class="gt_row gt_right">203.80</td>
<td class="gt_row gt_right">190.598</td>
<td class="gt_row gt_right">25.44</td>
<td class="gt_row gt_right">5.2776</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">Good</td>
    </tr>
    <tr><td class="gt_row gt_right">2002</td>
<td class="gt_row gt_right">123.75</td>
<td class="gt_row gt_right">84.263</td>
<td class="gt_row gt_right">23.95</td>
<td class="gt_row gt_right">1.0472</td></tr>
    <tr><td class="gt_row gt_right">2006</td>
<td class="gt_row gt_right">137.33</td>
<td class="gt_row gt_right">85.816</td>
<td class="gt_row gt_right">26.37</td>
<td class="gt_row gt_right">0.4509</td></tr>
    <tr><td class="gt_row gt_right">2010</td>
<td class="gt_row gt_right">220.00</td>
<td class="gt_row gt_right">196.099</td>
<td class="gt_row gt_right">26.04</td>
<td class="gt_row gt_right">3.5473</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">Poor</td>
    </tr>
    <tr><td class="gt_row gt_right">2002</td>
<td class="gt_row gt_right">13.75</td>
<td class="gt_row gt_right">9.570</td>
<td class="gt_row gt_right">2.40</td>
<td class="gt_row gt_right">1.1690</td></tr>
    <tr><td class="gt_row gt_right">2006</td>
<td class="gt_row gt_right">15.00</td>
<td class="gt_row gt_right">6.928</td>
<td class="gt_row gt_right">2.30</td>
<td class="gt_row gt_right">0.9539</td></tr>
    <tr><td class="gt_row gt_right">2010</td>
<td class="gt_row gt_right">27.40</td>
<td class="gt_row gt_right">27.318</td>
<td class="gt_row gt_right">2.36</td>
<td class="gt_row gt_right">0.7701</td></tr>
  </tbody>
  
  
</table>
</div>

**The response proportions in there years do not have obvious variance,
while the sample size is increase from 2002 to 2010.**

### 3.3.4 Create a ggplot that communicates the results/trends from the table above and stands on its own.

``` r
brfss_clean %>% filter(Year %in% c("2002", "2006", "2010"),
                       State == "MN",
                       Response %in% c("Excellent", "Good", "Poor")) %>%
  group_by(Response, Year) %>%
  summarise(across(c(Sample_Size, Response_Prop), 
                   list(mean = mean,
                        sd = sd),
                   na.rm = FALSE,
                   .names = "{.col}.{.fn}")) %>%
  pivot_longer(cols = c(Sample_Size.mean, Sample_Size.sd, Response_Prop.mean, Response_Prop.sd),
               names_to = "Stat",
               values_to = "Value") %>%
  ggplot(aes(x = Year, y = Value, group = Response, color = Response)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Stat) +
  labs(x = "Value",
       y = "Statistics",
       title = "Response Proportion and Sample Size in MN on 2002, 2006 and 2010")
```

<img src="PubH7462_HW2_panxx408_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" style="display: block; margin: auto;" />
