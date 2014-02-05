"""
https://code.google.com/p/appengine-pipeline/wiki/GettingStarted
"""

from google.appengine.api import memcache, urlfetch

from pipeline import pipeline

import json, logging, webapp2 yaml

"""
http://stackoverflow.com/questions/3973325/remove-all-html-in-python
"""

def strip_tags(url):
    from lxml import html
    from lxml.html.clean import clean_html
    tree=html.parse(url)
    tree=clean_html(tree)
    text=tree.getroot().text_content()
    return text.split()

def random_key(n=32):
    import random
    charset=[]
    for offset in [65, 97]:
        charset+=[chr(i+offset)
                  for i in range(26)]
    return "".join([charset[int(random.random()*len(charset))]
                    for i in range(n)])

class MapPipeline(pipeline.Pipeline):

    def run(self, stage_id, url):
        logging.info("Running map pipeline for: %s" % url)
        words=strip_tags(url)
        map_id=random_key()
        memcache.set("words/%s/%s" % (stage_id, map_id),
                     json.dumps(words))
        return map_id

    def finalized(self):
        pass

"""
what if data isn't found in memcache ? how to handle errors ?
"""

class ReducePipeline(pipeline.Pipeline):

    def run(self, stage_id, *map_ids):
        logging.info("Running reduce pipeline")
        count=0
        for map_id in map_ids:
            words=json.loads(memcache.get("words/%s/%s" % (stage_id, map_id)))
            count+=len(words)
        results={"total": count}
        memcache.set("results/%s" % stage_id, json.dumps(results))

    def finalized(self):
        pass

class MapReducePipeline(pipeline.Pipeline):

    def run(self, stage_id):
        logging.info("Running mapreduce pipeline")
        urls=json.loads(memcache.get("request/%s" % stage_id))
        map_ids=[(yield MapPipeline(stage_id, url))
                 for url in urls]
        yield ReducePipeline(stage_id, *map_ids)

    def finalized(self):
        pass

SampleUrls=yaml.load("""
- http://www.bbc.co.uk/news
- http://www.guardian.co.uk/
""")

"""
curl http://localhost:8080/wordcount/start
"""

class StartHandler(webapp2.RequestHandler):

    def get(self):    
        stage_id=random_key()
        memcache.set("request/%s" % stage_id, json.dumps(SampleUrls))
        stage=MapReducePipeline(stage_id)
        stage.start(queue_name="default")
        self.response.headers['Content-Type']='text/plain'
        self.response.write(stage_id)

"""
curl "http://localhost:8080/wordcount/results?id=#{id}"
"""


class ResultsHandler(webapp2.RequestHandler):
    
    def get(self):
        try:
            stage_id=self.request.get("id")
            if stage_id=='':
                raise RuntimeError("Please supply id")
            results=memcache.get("results/%s" % stage_id)
            if results in [None, ""]:
                raise RuntimeError("Results not found")
            self.response.headers['Content-Type']='application/json'
            self.response.write(results)
        except RuntimeError, error:    
            self.response.set_status(400)
            self.response.headers['Content-Type']='text/plain'
            self.response.write(str(error))

Routing=[('/wordcount/start.*', StartHandler),
         ('/wordcount/results.*', ResultsHandler)]

app=webapp2.WSGIApplication(Routing)
