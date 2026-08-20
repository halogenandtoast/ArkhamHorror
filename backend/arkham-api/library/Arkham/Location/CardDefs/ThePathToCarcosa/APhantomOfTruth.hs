module Arkham.Location.CardDefs.ThePathToCarcosa.APhantomOfTruth where

import Arkham.Location.CardDefs.Import

canalSaintMartin :: CardDef
canalSaintMartin =
  victory 1
    $ location
      "03216"
      "Canal Saint-Martin"
      [Paris]
      Equals
      [Square, T, Moon]
      APhantomOfTruth

gardensOfLuxembourg :: CardDef
gardensOfLuxembourg =
  victory 1
    $ location
      "03220"
      "Gardens of Luxembourg"
      [Paris]
      Star
      [Circle, Heart, Plus]
      APhantomOfTruth

gareDOrsay :: CardDef
gareDOrsay =
  location
    "03214"
    "Gare d'Orsay"
    [Paris, Rail]
    Heart
    [Diamond, Circle, Star]
    APhantomOfTruth

grandGuignol :: CardDef
grandGuignol =
  victory 1
    $ location
      "03211"
      ("Grand Guignol" <:> "Theatre of the Great Puppet")
      [Paris]
      Triangle
      [Diamond, Square]
      APhantomOfTruth

leMarais217 :: CardDef
leMarais217 =
  location
    "03217"
    "Le Marais"
    [Paris, Rail]
    Moon
    [Square, Equals, T, Plus]
    APhantomOfTruth

leMarais218 :: CardDef
leMarais218 =
  location
    "03218"
    "Le Marais"
    [Paris, Rail]
    Moon
    [Square, Equals, T, Plus]
    APhantomOfTruth

montmartre209 :: CardDef
montmartre209 =
  location
    "03209"
    "Montmartre"
    [Paris, Rail]
    Square
    [Diamond, Triangle, Equals, Moon]
    APhantomOfTruth

montmartre210 :: CardDef
montmartre210 =
  location
    "03210"
    "Montmartre"
    [Paris, Rail]
    Square
    [Diamond, Triangle, Equals, Moon]
    APhantomOfTruth

montparnasse :: CardDef
montparnasse =
  location
    "03208"
    "Montparnasse"
    [Paris, Rail]
    Circle
    [Heart, Star, Plus]
    APhantomOfTruth

notreDame :: CardDef
notreDame =
  location
    "03219"
    "Notre-Dame"
    [Paris, Rail]
    Plus
    [Circle, Moon, Star]
    APhantomOfTruth

operaGarnier212 :: CardDef
operaGarnier212 =
  location
    "03212"
    "Opéra Garnier"
    [Paris, Rail]
    Diamond
    [Triangle, Square, Heart]
    APhantomOfTruth

operaGarnier213 :: CardDef
operaGarnier213 =
  location
    "03213"
    "Opéra Garnier"
    [Paris, Rail]
    Diamond
    [Triangle, Square, Heart]
    APhantomOfTruth

pereLachaiseCemetery :: CardDef
pereLachaiseCemetery =
  victory 1
    $ location
      "03215"
      "Père Lachaise Cemetery"
      [Paris]
      T
      [Equals, Moon]
      APhantomOfTruth
