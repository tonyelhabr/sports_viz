
#%%
from sportsipy.fb.schedule import Schedule

tottenham = Schedule('Tottenham Hotspur')
for game in tottenham.schedule:
    print(game.datetime)  # Prints the datetime for each game

#%%