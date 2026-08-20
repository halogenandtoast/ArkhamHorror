module Arkham.Location.CardDefs.TheForgottenAge.ShatteredAeons where

import Arkham.Location.CardDefs.Import

aPocketInTime :: CardDef
aPocketInTime =
  victory 1
    $ singleSided
    $ location
      "04330"
      "A Pocket in Time"
      [Extradimensional]
      Star
      [Diamond, Equals]
      ShatteredAeons

atlantis :: CardDef
atlantis =
  singleSided
    $ location
      "04333"
      "Atlantis"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

cityOfTheUnseen :: CardDef
cityOfTheUnseen =
  singleSided
    $ location
      "04329"
      "City of the Unseen"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

mu :: CardDef
mu =
  victory 1
    $ singleSided
    $ location
      "04332"
      "Mu"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

nexusOfNKai :: CardDef
nexusOfNKai =
  location
    "04324"
    ("Nexus of N'kai" <:> "Unraveling the Threads")
    [Ancient, Ruins]
    Diamond
    [Droplet, Star]
    ShatteredAeons

plateauOfLeng :: CardDef
plateauOfLeng =
  singleSided
    $ location
      "04336"
      "Plateau of Leng"
      [Shattered, PresentDay]
      Equals
      [Star]
      ShatteredAeons

pnakotus :: CardDef
pnakotus =
  victory 1
    $ singleSided
    $ location
      "04334"
      "Pnakotus"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

ruinsOfNewYork :: CardDef
ruinsOfNewYork =
  singleSided
    $ location
      "04331"
      "Ruins of New York"
      [Shattered, Future, Ruins]
      Equals
      [Star]
      ShatteredAeons

shoresOfRlyeh :: CardDef
shoresOfRlyeh =
  singleSided
    $ location
      "04328"
      "Shores of R'lyeh"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

valusia :: CardDef
valusia =
  victory 1
    $ singleSided
    $ location
      "04335"
      "Valusia"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

yuggoth :: CardDef
yuggoth =
  singleSided
    $ location "04327" "Yuggoth" [Otherworld] Droplet [Diamond] ShatteredAeons
