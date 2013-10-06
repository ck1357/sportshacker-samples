"""
script to crawl Stan James event tree
"""

from lxml import etree

import urllib

UrlPatterns={
    "root": "http://www.stanjames.com/cache/boNavigationList/541/UK/%s.xml",
    "group": "http://www.stanjames.com/cache/marketgroup/UK/%s.xml",
    "event": "http://www.stanjames.com/cache/event/UK/%s.xml",
    "market": "http://www.stanjames.com/cache/market/UK/%s.xml"
    }

def get_categories_for_sport(sport):    
    url=UrlPatterns["root"] % sport["id"]
    doc=etree.fromstring(urllib.urlopen(url).read())
    return [{"name": el.xpath("name")[0].text,
             "id": el.xpath("idfwbonavigation")[0].text}
            for el in doc.xpath("//bonavigationnode")]

def get_groups_for_category(category):
    url=UrlPatterns["root"] % category["id"]
    doc=etree.fromstring(urllib.urlopen(url).read())
    return [{"name": el.xpath("name")[0].text,
             "id": el.xpath("idfwmarketgroup")[0].text}
            for el in doc.xpath("//marketgroup")]

def get_events_for_group(group):
    url=UrlPatterns["group"] % group["id"]
    doc=etree.fromstring(urllib.urlopen(url).read())
    return [{"name": el.xpath("eventname")[0].text,
             "id": el.xpath("idfoevent")[0].text}
            for el in doc.xpath("//market")]

def get_markets_for_event(event):
    url=UrlPatterns["event"] % event["id"]
    doc=etree.fromstring(urllib.urlopen(url).read())
    return [{"name": el.xpath("name")[0].text,
             "id": el.xpath("idfomarket")[0].text}
            for el in doc.xpath("//market")]

def get_selections_for_market(market):
    url=UrlPatterns["market"] % market["id"]
    doc=etree.fromstring(urllib.urlopen(url).read())
    return [{"name": el.xpath("name")[0].text,
             "id": el.xpath("idfoselection")[0].text,
             "price": "%s/%s" % (el.xpath("currentpriceup")[0].text,
                                 el.xpath("currentpricedown")[0].text)}
            for el in doc.xpath("//selection")]

import re, time

SportIds={"Football": "58974.2"}

Handlers=[get_categories_for_sport,
          get_groups_for_category,
          get_events_for_group,
          get_markets_for_event,
          get_selections_for_market]

def crawl_events(path, wait=1):
    tokens=path.split("/")
    if tokens[0] not in SportIds:
        raise RuntimeError("Sport not found")
    stack, results = [], []
    stack.append(({"name": tokens[0],
                   "id": SportIds[tokens[0]]}, 
                  [tokens[0]],
                  0))
    while True:
        if stack==[]:
            break
        head, stack = stack[0], stack[1:]
        parent, path, depth = head
        print "/".join(path)
        handler=Handlers[depth]
        if depth < len(tokens)-1:
            stack+=[(result, 
                     path+[result["name"]],
                     depth+1)
                    for result in handler(parent)
                    if re.search(tokens[depth+1], result["name"])]
        else:
            for result in handler(parent):
                result["path"]=path+[result["name"]]
                results.append(result)
        time.sleep(wait)
    return results

if __name__=="__main__":
    try:
        results=crawl_events("Football/^English Premier League$/Matches/ v /Match Betting")
        for result in results:
            print ("%s/%s" % (result["path"][-3], 
                              result["path"][-1]),
                   result["price"])
    except RuntimeError, error:
        print "Error: %s" % str(error)
