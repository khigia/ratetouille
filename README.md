ratetouille
===========

Create collections of items to rate.


Motivations:
 - I needed a simple way to display few ideas and let people rate/vote.
   This is similar to a poll survey, but this let people view and edit their answer and result in some kind of democratic priority list.
 - I wanted a project to test Yesod.

On second thought: this could be implemented as a service without web UI.


Run through cabal:
  - edit config/settings.yml
  - cabal build
  - ./dist/build/ratetouille/ratetouille Development
