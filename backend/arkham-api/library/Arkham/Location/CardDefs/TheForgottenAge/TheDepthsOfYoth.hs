module Arkham.Location.CardDefs.TheForgottenAge.TheDepthsOfYoth where

import Arkham.Location.CardDefs.Import

abandonedSite :: CardDef
abandonedSite =
  singleSided
    $ location
      "04294"
      "Abandoned Site"
      [Ancient, Cave, Yoth]
      Square
      [Equals, Diamond, Triangle, T, Squiggle]
      TheDepthsOfYoth

bridgeOverNKai :: CardDef
bridgeOverNKai =
  singleSided
    $ location
      "04292"
      "Bridge over N'kai"
      [Ancient, Cave, Yoth]
      Heart
      [Equals, Circle, Droplet, Hourglass, Squiggle]
      TheDepthsOfYoth

brightCanyon :: CardDef
brightCanyon =
  singleSided
    $ location
      "04295"
      "Bright Canyon"
      [Ancient, Cave, Yoth]
      Circle
      [Droplet, Squiggle, T, Heart, Triangle]
      TheDepthsOfYoth

brokenPassage :: CardDef
brokenPassage =
  singleSided
    $ location
      "04293"
      "Broken Passage"
      [Ancient, Cave, Yoth]
      Squiggle
      [Circle, Droplet, Hourglass, Square, Heart]
      TheDepthsOfYoth

cavernsOfYoth :: CardDef
cavernsOfYoth =
  singleSided
    $ location
      "04290"
      "Caverns of Yoth"
      [Ancient, Cave, Yoth]
      Droplet
      [Circle, Hourglass, Heart, Diamond, Squiggle]
      TheDepthsOfYoth

cityOfTheSerpents :: CardDef
cityOfTheSerpents =
  vengeance 2
    $ singleSided
    $ location
      "04287"
      "City of the Serpents"
      [Ancient, Cave, Yoth]
      Diamond
      [Equals, Droplet, Triangle, T, Square]
      TheDepthsOfYoth

crumblingPrecipice :: CardDef
crumblingPrecipice =
  singleSided
    $ location
      "04289"
      "Crumbling Precipice"
      [Ancient, Cave, Yoth]
      Hourglass
      [Equals, Squiggle, Heart, T, Droplet]
      TheDepthsOfYoth

forkedPath :: CardDef
forkedPath =
  singleSided
    $ location
      "04291"
      "Forked Path"
      [Ancient, Cave, Yoth]
      T
      [Circle, Diamond, Hourglass, Square, Triangle]
      TheDepthsOfYoth

hallOfHeresy :: CardDef
hallOfHeresy =
  vengeance 2
    $ singleSided
    $ location
      "04288"
      "Hall of Heresy"
      [Ancient, Cave, Yoth]
      Triangle
      [Equals, Diamond, Circle, Square, T]
      TheDepthsOfYoth

stepsOfYoth :: CardDef
stepsOfYoth =
  singleSided
    $ location
      "04286"
      "Steps of Yoth"
      [Ancient, Forgotten, Yoth]
      Equals
      [Hourglass, Square, Triangle, Diamond, Heart]
      TheDepthsOfYoth
