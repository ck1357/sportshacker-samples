import lxml.html, re, urllib

def parse_fractional_quote(text):
    tokens=[int(tok) for tok in text.split("/")]
    if len(tokens)==1:
        tokens.append(1)
    return 100/(1+tokens[0]/float(tokens[1]))

def get_prices(url):
    doc=lxml.html.fromstring(urllib.urlopen(url).read())
    table=doc.xpath("//table[@class='eventTable ']")[0]
    rows=table.xpath("tbody/tr")
    items={}
    for row in rows:
        oddscheckerid=row.attrib["data-participant-id"]
        name=None
        for td in row.xpath("td"):
            if not "id" in td.attrib:
                continue
            suffix=td.attrib["id"].split("_")[-1]
            if suffix=="name":
                name=td.text
                continue
            if (suffix=="best" or
                td.text in [None, "SP"] or
                re.search("^\\d+\\-\\d+$", td.text)):
                continue
            items.setdefault(name, {"name": name,
                                    "bookmaker": None,
                                    "price": 1e10})
            price=parse_fractional_quote(td.text)
            if price < items[name]["price"]:
                items[name]["price"]=price
                items[name]["bookmaker"]=suffix
    return sorted(items.values(),
                  key=lambda x: -x["price"])

if __name__=="__main__":
    print get_prices("http://www.oddschecker.com/football/english/premier-league/top-3-finish")
