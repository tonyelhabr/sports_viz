
This repo stores my code and plots for ports-related plots.

<blockquote class="twitter-tweet">

<p lang="en" dir="ltr">

With under two weeks left before the NBA starts up again, here's a look
at projected playoff seedings for the Western Conference. (Eastern
Conference is mostly locked in🥱.)<br><br>Data from
<a href="https://twitter.com/JacobEGoldstein?ref_src=twsrc%5Etfw">@JacobEGoldstein</a>.
Inspiration from
<a href="https://twitter.com/experimental361?ref_src=twsrc%5Etfw">@experimental361</a>.
<a href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw">\#rstats</a>
<a href="https://t.co/G97IsVsSjY">pic.twitter.com/G97IsVsSjY</a>

</p>

— Tony (@TonyElHabr)
<a href="https://twitter.com/TonyElHabr/status/1284452770305781761?ref_src=twsrc%5Etfw">July
18, 2020</a>

</blockquote>

<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

![NBA Western Conference 2020 League Restart Playoff Seeding
Probabilities Bar Chart](plots/nba_seed_p.png
"NBA Western Conference 2020 League Restart Playoff Seeding Probabilities Bar Chart")

<blockquote class="twitter-tweet">

<p lang="en" dir="ltr">

Here's a look at how
<a href="https://twitter.com/hashtag/EPL?src=hash&amp;ref_src=twsrc%5Etfw">\#EPL</a>
teams performed since the league
reset.<a href="https://twitter.com/hashtag/MUFC?src=hash&amp;ref_src=twsrc%5Etfw">\#MUFC</a>
and
<a href="https://twitter.com/hashtag/THFC?src=hash&amp;ref_src=twsrc%5Etfw">\#THFC</a>
(and
<a href="https://twitter.com/hashtag/SaintsFC?src=hash&amp;ref_src=twsrc%5Etfw">\#SaintsFC</a>
) pulled off a string of outcomes that put them above the 85th
percentile of outcomes from a simulated 9 game sample of their own
outcomes.
<a href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw">\#rstats</a>
<a href="https://t.co/RsUYO1OUcL">pic.twitter.com/RsUYO1OUcL</a>

</p>

— Tony (@TonyElHabr)
<a href="https://twitter.com/TonyElHabr/status/1287460387466682369?ref_src=twsrc%5Etfw">July
26, 2020</a>

</blockquote>

<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

![English Premier League 2019-20 First 30 Games Points Minus Expected
Points Lollipop Chart](plots/epl_before_break_pts_minus_xpts.png
"English Premier League 2019-20 First 30 Games Points Minus Expected Points Lollipop Chart")

![English Premier League 2019-20 First 30 Games Points vs. Expected
Points Scatter Plot](plots/epl_before_break_pts_vs_xpts.png
"English Premier League 2019-20 First 30 Games Points vs. Expected Points Scatter Plot")

![English Premier League 2019-20 Last 9 Games Simulated Points
vs. Actual Points Ridge Plot](plots/epl_after_break_pts_sim.png
"English Premier League 2019-20 Last 9 Games Simulated Points vs. Actual Points Ridge Plot")

<blockquote class="twitter-tweet">

<p lang="en" dir="ltr">

Simple and pertinent
<a href="https://twitter.com/hashtag/viz?src=hash&amp;ref_src=twsrc%5Etfw">\#viz</a>
this week. Which social justice messages are most
<a href="https://twitter.com/hashtag/NBA?src=hash&amp;ref_src=twsrc%5Etfw">\#NBA</a>
🏀players choosing to wear on their jerseys?
<a href="https://twitter.com/hashtag/BLM?src=hash&amp;ref_src=twsrc%5Etfw">\#BLM</a>
and “equality” are by far the most
prevalent.<a href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw">\#rstats</a>
<a href="https://t.co/JXCEhZtvNg">pic.twitter.com/JXCEhZtvNg</a>

</p>

— Tony (@TonyElHabr)
<a href="https://twitter.com/TonyElHabr/status/1289919973536616450?ref_src=twsrc%5Etfw">August
2, 2020</a>

</blockquote>

<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

![2020 NBA Restart Social Justice Message Bar
Chart](plots/nba_social_justice.png
"2020 NBA Restart Social Justice Message Bar Chart")

![](plots/04_ucl_npg90_adj.utf8.pdf)<!-- -->

![Competition-Adjusted Non-Penalty Goals Per 90 Minutes Dumbbell
Char](plots/ucl_npg90_adj.png
"Competition-Adjusted Non-Penalty Goals Per 90 Minutes Dumbbell Chart")

## Future Ideas

  - Stability of goal-scoring rate and/or xG over-performance

  - Madden rating EDA with UMAP + kmeans clustering (for optimal \# of
    UMAP groups).

  - In which sport is the leading team most likely to further their lead
    near the end of the game due to an opposition error,
    overly-aggressive tactics by the opposition, etc.? (End of game
    depends on the sport. Ideally, we could use a win probability model
    to help idenitfy end-of-game context, although it would still need
    some adjustment.) Extension: In which sport are gamblers most likely
    to lose/win due to a back-door cover?

## Future data sets

  - “Standardized” soccer shot data locations:
    <https://kloppy.pysport.org/changelog/#110-2020-08-07>

  - xG soccer model: <https://github.com/ML-KULeuven/soccer_xg>

  - Transfer market data: <https://github.com/ewenme/transfers>

  - FIFA data: <https://github.com/RobWHickman/fifadb>

  - Champions League over-performers:
    <https://cdn.substack.com/image/fetch/c_limit,f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fbucketeer-e05bbc84-baa3-437e-9518-adb32be77984.s3.amazonaws.com%2Fpublic%2Fimages%2Fa1354a2f-8928-453f-a5ed-df142021ceb7_1004x935.png>
