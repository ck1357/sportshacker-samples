"""
NB currently returns date, score and best match odds probabilities for bookmakers listed below
"""

"""
http://football-data.co.uk/notes.txt
"""

import csv, datetime, urllib, yaml

Bookmakers=yaml.load("""
B365: Bet365
LB: Ladbrokes
SJ: Stan James
PS: Pinnacle
VC: Victor Chandler
WH: William Hill
""")

Outcomes=["H", "A", "D"]

def parse_date(text):
    day, month, year = [int(tok) 
                        for tok in text.split("/")]
    year+=2000
    return datetime.date(*[year, month, day])

def aggregate_probabilities(item):
    prices={}
    for bookiecode in Bookmakers.keys():        
        for attr in Outcomes:
            key=bookiecode+attr
            if (key not in item or
                item[key] in ['']):
                continue
            price=float(item[key])            
            if (attr not in prices or
                price > prices[attr]):
                prices[attr]=price
    probs=[]
    for attr in Outcomes:
        if attr not in prices:
            probs.append(None)
        else:
            probs.append(1/float(prices[attr]))
    if None in probs:
        return None
    else:
        return probs

def get_results(url):
    reader=csv.reader(urllib.urlopen(url))
    titles=reader.next()
    items=[dict([(title, value)
                 for title, value in zip(titles, row)])
           for row in reader]
    results=[{"date": parse_date(item["Date"]),
              "home_team": item["HomeTeam"],
              "away_team": item["AwayTeam"],
              "score": (int(item["FTHG"]),
                        int(item["FTAG"])),
              "probabilities": aggregate_probabilities(item)}
             for item in items
             if (item["FTHG"] not in [None, ""] and
                 item["FTAG"] not in [None, ""])] # NB can be blank
    for result in results:
        result["overround"]=sum(result["probabilities"])
    return results

if __name__=="__main__":
    print get_results("http://www.football-data.co.uk/mmz4281/1314/E0.csv")


