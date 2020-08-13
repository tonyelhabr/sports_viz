#%%
from pbpstats.client import Client

settings = {
    # 'dir': 'c:/users/aelhabr/downloads/data/data',
    # 'dir': '/response_data',
    'Boxscore': {
        'source': 'web',
        'data_provider': 'stats_nba'
    },
    'Possessions': {
        'source': 'web',
        'data_provider': 'stats_nba'
    },
}
client = Client(settings)

game = client.Game('0020000006')
#%%
game

#%%
from pbpstats.resources.enhanced_pbp import FieldGoal
shot_dists = []
for possession in game.possessions.items:
    for possession_event in possession.events:
        # if isinstance(possession_event, FieldGoal) and not possession_event.is_made and possession_event.shot_value == 2:
        #     shot_dists.append(possession_event.distance)
        print(possession_event)
# print(sum(shot_dists) / len(shot_dists))

#%%
