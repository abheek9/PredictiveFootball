
# coding: utf-8

# In[164]:

# importing the packages used

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


# In[136]:


### cleaning teamTiers.txt into a DataFrame and saving it as a csv file

counter = 0
d = dict()
# read in the text file
f = open("teamTiers.txt", 'r')
teamName = ""
# read the file line by line
for line in f:
    # split the line on whitespace
    s = line.split()
    # remove the first element since it 
    s.remove('[1]')
    # see if we are reading in a name line
    if counter % 2 == 0:
        # check if it is one-word team name
        if len(s) == 1:
            teamName = s[0]
        else:
            teamName = " ".join(s)
        # get rid of the quotation marks
        teamName = teamName[1:-1]
    # if it is a line that contains the league codes
    else:
        # first get the int versions of the league numbers
        l = [int(i) for i in s]
        l.remove(0)
        # then create the binary list that has 1 for the league played or 0 otherwise
        binarylist = [0 for i in xrange(0,4)]
        for i in xrange(1,5):
            if i in l:
                binarylist[i-1] = 1
        # add the binary list for team to the dict
        d[teamName] = binarylist
    # increment the counter
    counter += 1
f.close() # close the file
d # print the dictionary


# In[137]:

# format the dictionary into a DataFrame and name the columns appropriately
df = pd.DataFrame.from_dict(d, orient='index')
df.columns = ['1', '2', '3', '4']
df.index.name = "Team Name"
# save it as a csv
df.to_csv('teamTiers_cleaned.csv')
df[:10]


# In[138]:


### cleaning teamsOverTime.txt into a DataFrame and saving it as a csv file

counter = 0 # this will keep track of the line that is read
d2 = dict()
# open the file
f = open("teamsOverTime.txt", 'r')
teamName = ""
# read it line by line 
for line in f:
    s = line.split()
    # check if we are reading a new entry line or a continuation of output
    if s[0] == '[1]':
        s.remove('[1]')
        # see if we are reading in a name line
        if counter % 2 == 0:
            # check if it is one-word team name
            if len(s) == 1:
                teamName = s[0]
            else:
                teamName = " ".join(s)
            # get rid of the quotation marks
            teamName = teamName[1:-1]
        # if it is a line of leagues
        else:
            # first get the int versions of the league numbers
            l = [int(i) for i in s]
            l.remove(0)
            # create binary responses
            binarylist = [0 for i in xrange(0,127)]
            for i in l:
                binarylist[i-1888] = 1
            # add the binary list for team to the dict
            d2[teamName] = binarylist
        # increment the counter
        counter += 1
    else:
        # if it is a continuation of output, just flip the appropriate responses to 1
        x = s[0]
        s.remove(x) # get rid of the useless line number
        l = [int(i) for i in s]
        for i in l:
            d2[teamName][i-1888] = 1
f.close()
d2


# In[139]:

# format the dictionary into a DataFrame and name the columns appropriately
df2 = pd.DataFrame.from_dict(d2, orient='index')
df2.columns = [str(i) for i in xrange(1888,2015)] 
df2.index.name = "Team Name"
df2.to_csv('teamsOverTime_cleaned.csv')
df2


# In[160]:

# Frequency Counts:
# this function creates count frequency per bucket from given binary responses
#
# params:
# bindic = dictionary of binary responses 
# num_buckets = how many buckets we want the counts to be aggregated over
#
# output:
# total_counts = dictionary that maps freq counts to a given bucket
def bucket_per_range(bindic, num_buckets):
    # first get the count dict
    x = bindic.values()[0]
    cntdic = {i:0 for i in xrange(1,len(x)+1)}
    for v in bindic.values():
        cnt = sum(v)
        cntdic[cnt] += 1
    m = len(cntdic.values()) # number of possible sums
    n = m / num_buckets # ideal size of the window per bucket
    y = 1
    # important edge case: what if the # of buckets = # of entries?
    # then there is no entry at 0! 
    # the y-variable below deals with that
    if n == 1:
        y = 0
    # initialize total counts as zero values first
    total_counts = {i:0 for i in xrange(1-y,num_buckets+1-y)}
    # fill up the buckets with the right count values
    for i in xrange(1, m+1):
        place = (i / n)
        # place any last leftovers into the last bucket
        if place >= num_buckets - y:
            place = num_buckets - y
        total_counts[place] += cntdic[i] 
    # check if both counts match (there should 147 = # of teams)
    print sum(cntdic.values()) == sum(total_counts.values())
    return total_counts


# In[188]:

# bucket for the number of leagues participated in
d_league = bucket_per_range(d, 4)
nb = 10 # number of buckets
nt = 127 # number of possible total years
# bucket for total number of years
d_year = bucket_per_range(d2, nb)
d_year


# In[218]:

### code for plotting the d_league values
plt.style.use('ggplot')
plt.bar(range(len(d_league)), d_league.values(), align='center')
plt.xticks(range(len(d_league)), ['1 League', '2 Leagues', '3 Leagues', '4 Leagues'])
plt.suptitle("Aggregate Relegation and Promotion Information", fontsize=18)
plt.xlabel('Total Leagues Participated In')
plt.ylabel('# of Teams')
plt.show()


# In[195]:

# create x-axis titles for a given bucket range
titles = ["" for i in xrange(0,nb)]
runtot = 0
for i in xrange(0,nb):
    runtot += 1
    x = runtot
    y = 0
    if i == nb - 1:
        y = nt
    else:
        y = runtot + 12
    titles[i] = "{}-{} yrs".format(x, y)
    runtot += 12
titles


# In[220]:

### code for plotting the d_year values
plt.style.use('ggplot')
plt.bar(range(len(d_year)), np.log(d_year.values()), align='center')
plt.xticks(range(len(d_year)), titles)
plt.suptitle("Distribution of Teams by Activity Over Time, 1888-2014", fontsize=18)
plt.xlabel('Total # Years of Activity')
plt.ylabel('Log-Scaled # of Teams')
plt.show()


# In[201]:

## count how many teams are active in a given year
dfmat = df2.as_matrix()
teams_per_year = [np.sum(dfmat[:,i]) for i in xrange(0,nt)]
teams_per_year


# In[222]:

### plot the teams_per_year as a time series 
ts = pd.Series(teams_per_year, index=pd.date_range('1888', '2014', freq='AS'))
plt.style.use('ggplot')
ts.plot()
plt.suptitle("Number of Teams Participating per Given Year", fontsize=18)
plt.xlabel('Year (1888-2014)')
plt.ylabel('# of Teams Playing')
plt.show()


# In[274]:

### split the score distributions according to home team score

df_scores = pd.DataFrame.from_csv('EnglishPremLeagueScoreDist.csv')
home_scores_dict = {i:0 for i in xrange(0,14)}
for c in df_scores.columns:
    l = c.split('-')
    # count how many possible away-team-scores there are per home team score
    home_scores_dict[int(l[0])] += 1
cntr = 0
# using the above info, split the dataset
dist_dic = {i:[0 for j in xrange(0,home_scores_dict[i])] for i in xrange(0,14)}
dfm = df_scores.as_matrix()[0]
total = 0
for k,v in home_scores_dict.iteritems():
    for i in xrange(0,v):
        dist_dic[k][i] = dfm[total] 
        total += 1
print dist_dic.values()
#dist_dic


# In[279]:

plt.style.use('ggplot')
plt.figure(1)                # the first figure
plt.subplot(311)             # the first subplot in the first figure
plt.plot([j for j in xrange(0,home_scores_dict[0])], dist_dic[0])
plt.xlabel('Goals Scored by Away Team')
plt.ylabel('# of Matches')
plt.subplot(312)             # the second subplot in the first figure
plt.plot([j for j in xrange(0,home_scores_dict[1])], dist_dic[1])
plt.suptitle("Score Distribution in English Premier League", fontsize=18)
plt.ylabel('# of Matches')
plt.subplot(313)             # the first subplot in the first figure
plt.plot([j for j in xrange(0,home_scores_dict[2])], dist_dic[2])
plt.xlabel('Goals Scored by Away Team')
plt.ylabel('# of Matches')
plt.show()

