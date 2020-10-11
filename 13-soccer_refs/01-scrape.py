#%%
import numpy as np
import pandas as pd
import sportsreference as sr
from pyquery import PyQuery as pq

#%%
tm_name = 'Tottenham Hotspur'
# tm_id = '8cec06e1'
from sportsreference.fb.team import Team
tm = Team(tm_name)

#%%
from sportsreference.fb.schedule import Schedule

sched = Schedule(tm_name)
# sched = Schedule(361ca564)
print(sched._games)

#%%
squad_id = '361ca564'
# SQUAD_URL = 'https://fbref.com/en/squads/%s'
SQUAD_URL = 'https://fbref.com/en/squads/%s/%s'
doc = pq(SQUAD_URL.format(squad_id, 'Tottenham-Hospur-Stats'))
# doc = pq(SQUAD_URL % squad_id)
doc

#%%
import re
import requests
from datetime import datetime
from lxml.etree import ParserError, XMLSyntaxError
from pyquery import PyQuery as pq


def _remove_html_comment_tags(html):
    """
    Returns the passed HTML contents with all comment tags removed while
    keeping the contents within the tags.
    Some pages embed the HTML contents in comments. Since the HTML contents are
    valid, removing the content tags (but not the actual code within the
    comments) will return the desired contents.
    Parameters
    ----------
    html : PyQuery object
        A PyQuery object which contains the requested HTML page contents.
    Returns
    -------
    string
        The passed HTML contents with all comment tags removed.
    """
    return str(html).replace('<!--', '').replace('-->', '')


def _get_stats_table(html_page, div, footer=False):
    """
    Returns a generator of all rows in a requested table.
    When given a PyQuery HTML object and a requested div, this function creates
    a generator where every item is a PyQuery object pertaining to every row in
    the table. Generally, each row will contain multiple stats for a given
    player or team.
    Parameters
    ----------
    html_page : PyQuery object
        A PyQuery object which contains the requested HTML page contents.
    div : string
        The requested tag type and id string in the format "<tag>#<id name>"
        which aligns to the desired table in the passed HTML page. For example,
        "div#all_stats_table" or "table#conference_standings".
    footer : boolean (optional)
        Optionally return the table footer rows instead of the table header.
    Returns
    -------
    generator
        A generator of all row items in a given table.
    """
    stats_html = html_page(div)
    try:
        stats_table = pq(_remove_html_comment_tags(stats_html))
    except (ParserError, XMLSyntaxError):
        return None
    if footer:
        teams_list = stats_table('tfoot tr').items()
    else:
        teams_list = stats_table('tbody tr').items()
    return teams_list


#%%
schedule = _get_stats_table(doc, 'table#all_matchlogs_all')
print(schedule)

#%%
# doc('table#match_logs')
# print(doc)
stats_table = pq(_remove_html_comment_tags(doc))
# print(stats_table)
teams_list = stats_table('tbody tr').items()
print(teams_list)
#%%
from sportsreference.fb.roster import Roster

rost = Roster(tm_name)
print(rost.__dict__)

#%%
str(tm)
#%%
tm.home_draws
#%%
tm._away_games
#%%
print(tm.home_games)

#%%
for g in sched:
    print(g.date)
#%%
for g in tm._away_games:
    print(g.date)
#%%
from sportsreference.fb.schedule import SCHEDULE_SCHEME
print(SCHEDULE_SCHEME)
#%%
tm.__dict__
#%%
sched.__dict__

##
sportsreference.__version__
#%%
from sportsreference.fb.schedule import Schedule

tottenham = Schedule('Tottenham Hotspur')
for game in tottenham.schedule:
    print(game.datetime)  # Prints the datetime for each game
#%%
from sportsreference.fb.roster import Roster

rost = Roster('Tottenham Hotspur')
print(rost.__dict__)

#%%
