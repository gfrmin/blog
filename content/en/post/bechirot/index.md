---
title: "Israel's election for the 25th Knesset"
subtitle: "What just happened??"
author: admin
date: 2022-11-09
tags: ["israel", "elections", "data", "analysis"]
draft: true
---
<link href="{{< blogdown/postref >}}index_files/pagedtable/css/pagedtable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/pagedtable/js/pagedtable.js"></script>



After an unprecedented fifth general election in four years for Israel's Knesset (parliament), everyone was expecting another close result. Instead, the faction allied to the erstwhile prime minister Benjamin Netanyahu ended up with a strong (by Israeli standards) majority of 65 seats out of 120. Meanwhile, the venerable left-wing Meretz party will have no members in the Knesset for the first time since its founding in 199? [tktk], and the unsubtly racist Otzma Yehudit [Jewish Power] party, until a couple of years ago on the outer fringes of Israeli politics, has 7 [tktk] seats.

What happened? Tom Friedman stopped talking to the nearest taxi driver long enough to proclaim "the end of the Israel we knew" [tktk].

Those who didn't panic noted that the number of votes for the two factions (pro- and anti-Bibi) were very similar, and only a treacherous vote threshold caused two anti-Bibi parties (including the aforementioned venerable Meretz) to fail to win any seats, rather than a literal collapse to zero votes. Meretz were judged to have gotten xxx,xxx [tktk] valid votes, only xx,xxx votes away from clambering over the threshold.

Fair enough. However, this still doesn't answer the question: _why_ did Meretz fall below the threshold for the first time ever? And why did Otzma Yehudit grow so much? Could it be that the broken-clock columnist Friedman called the alarm correctly for once, and Israel is now a fascist wasteland?

Thank goodness we have granular voting data and the skills to analyze them to try to answer these questions.

Thanks to our friends at [Public Data Market](https://publicdatamarket.com), we have clean voting data from the 1999 elections to this most recent one. [Go sign up and download your own copy of the data for free now too](https://publicdatamarket.com/israeldata/bechirot).

To understand how parties' fortunes have changed, it's necessary to quickly remember how Israeli elections work.

In Israel, parties submit closed lists for election, and these lists can be combinations from multiple parties. The lists are represented by symbols composed of up to four Hebrew letters. (There are also symbols made up of equivalent Arab letters). For obvious reasons, these letters stay consistent between elections with few exceptions. For example, Bibi's Likud has used the symbol מחל‎ since its founding in 1973. Conversely, though, letters that represented defunct lists can be taken by others, so that a symbol does not necessarily represent the same group of parties.

Relatedly, a list of multiple parties can split up between elections, so that a much lower vote total for a particular symbol might be because it represents fewer parties on that occasion. For example, the aforementioned far-right Otzma Yehudit that grabbed the headlines for doing so well in this election was actually part of a list of multiple parties, two others of which were Religious Zionism and Noam. The Hebrew letter they stood under, ט, has been used for changing lists of Jewish nationalist parties. Indeed, Otzma Yehudit stood under the symbol נץ‎ in 2013 and 2020, and כף‎ in 2019.

Basically this means it's not trivial to exactly track parties' fortunes across elections. In order to try to do so, I will combine votes for parties that ever ran together on one list even during elections when they ran separately. This is not a perfect solution, as it is well-known that often lists of multiple parties receive fewer votes overall than the individual parties would receive if they were running alone. (This makes intuitive sense, for otherwise why would political parties ally in the election at all?).

First we confirm that the votes between the most recent two elections didn't change much _between_ the blocs. For simplicity, I only classify lists that received more than 1% of valid votes, counting Ayelet Shaked's previous and eventual political vehicles of Yamina and Jewish Home respectively (both ב) in the pro-Bibi camp, while erstwhile Bibi ally's Lieberman's Israel Beiteinu (ל) is put in the anti-Bibi camp.

With this calculation, the surface analysis is correct that the vote totals were very similar and very unchanged between the camps: the pro-Bibi camp (including Ayelet Shaked) received 48.3% of the valid votes in the 24th election, and 49.6% in the 25th election, while the anti-Bibi camp received 50.2% and 48.9%. Interestingly, as a percentage of the eligible voting population (so taking into account turnout), both camps grew, the Bibists from 32.4% to 34.8%, and the rest from 33.7% to 34.3%.

And looking at the individual lists -- after combining the votes for ת into כן (just as it did between the elections) and ד and ום into ודעם (as they were in the previous election) -- the changes in the vote share are:

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["List"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Vote change"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Valid % change"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pop % change"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"אמת","2":"-92775","3":"-2.4","4":"-1.5"},{"1":"ב","2":"-217061","3":"-5.0","4":"-3.3"},{"1":"ג","2":"31803","3":"0.2","4":"0.4"},{"1":"ודעם","2":"104769","3":"1.8","4":"1.4"},{"1":"ט","2":"290829","3":"5.7","4":"4.2"},{"1":"כן","2":"-68936","3":"-2.3","4":"-1.3"},{"1":"ל","2":"-34683","3":"-1.1","4":"-0.6"},{"1":"מחל","2":"48444","3":"-0.8","4":"0.2"},{"1":"מרצ","2":"-51425","3":"-1.4","4":"-0.9"},{"1":"עם","2":"26983","3":"0.3","4":"0.3"},{"1":"פה","2":"233323","3":"3.9","4":"3.1"},{"1":"שס","2":"76956","3":"1.1","4":"1.0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
