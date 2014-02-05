from google.appengine.ext import webapp

from pipeline import pipeline

Routing=pipeline.create_handlers_map()

app=webapp.WSGIApplication(Routing)

