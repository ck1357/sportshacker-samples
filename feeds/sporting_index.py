import lxml.html, json, re, urllib

LivePricingUrl="http://livepricing.sportingindex.com/LivePricing.svc/jsonp/GetLivePricesByMeeting?meetingKey="

def get_prices(url):        
    doc=lxml.html.fromstring(urllib.urlopen(url).read())
    ids=dict([(li.attrib["key"], re.sub(" Points$", "", li.xpath("span[@class='markets']")[0].text))
              for li in doc.xpath("//ul[@class='prices']/li")
              if "key" in li.attrib])
    quotes=json.loads(urllib.urlopen(LivePricingUrl+url.split("/")[-2]).read())
    return [{"name": ids[quote["Key"]],
             "so_far": tuple([int(tok) for tok in quote["SoFar"].split("/")]),
             "bid": quote["Sell"],
             "offer": quote["Buy"]}
            for quote in quotes["Markets"]]

if __name__=="__main__":
    print get_prices("http://www.sportingindex.com/spread-betting/football-domestic/premier-league/mm4.uk.meeting.4191659/premier-league-points-2013-2014")
