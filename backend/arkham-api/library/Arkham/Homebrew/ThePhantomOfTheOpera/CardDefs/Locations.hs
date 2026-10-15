{- | The Opera Garnier and the sewers beneath it.

The map is rebuilt three times. Setup puts three of the five Grand Foyers into
play (they share the Triangle symbol, so they connect to each other); act 1
collapses them to one and adds the [[OperaHouse]] rooms; act 2 lays three of the
five Paris Sewers in a line and adds The Phantom's Lair; act 3 adds Hidden
Parlor and Organ Chamber.

Paris Sewers prints its own connections ("to the left and right"), so it carries
no connecting symbols at all.
-}
module Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations where

import Arkham.Homebrew.ThePhantomOfTheOpera.Sets qualified as Set
import Arkham.Homebrew.ThePhantomOfTheOpera.Traits
import Arkham.Location.CardDefs.Import

grandFoyer :: CardCode -> CardDef
grandFoyer cardCode =
  location
    cardCode
    "Grand Foyer"
    [OperaHouse]
    Triangle
    [Triangle, Heart, Square]
    Set.ThePhantomOfTheOpera

grandFoyer_008 :: CardDef
grandFoyer_008 = grandFoyer ":the-phantom-of-the-opera:008"

grandFoyer_009 :: CardDef
grandFoyer_009 = grandFoyer ":the-phantom-of-the-opera:009"

grandFoyer_010 :: CardDef
grandFoyer_010 = grandFoyer ":the-phantom-of-the-opera:010"

grandFoyer_011 :: CardDef
grandFoyer_011 = grandFoyer ":the-phantom-of-the-opera:011"

grandFoyer_012 :: CardDef
grandFoyer_012 = grandFoyer ":the-phantom-of-the-opera:012"

auditorium :: CardDef
auditorium =
  location
    ":the-phantom-of-the-opera:013"
    "Auditorium"
    [OperaHouse]
    Heart
    [Triangle, Moon]
    Set.ThePhantomOfTheOpera

backstage :: CardDef
backstage =
  location
    ":the-phantom-of-the-opera:014"
    "Backstage"
    [OperaHouse]
    Moon
    [Heart, Square]
    Set.ThePhantomOfTheOpera

boxFive :: CardDef
boxFive =
  victory 1
    $ location
      ":the-phantom-of-the-opera:015"
      "Box Five"
      [OperaHouse]
      Square
      [Triangle, Moon]
      Set.ThePhantomOfTheOpera

hiddenParlor :: CardDef
hiddenParlor =
  victory 1
    $ location
      ":the-phantom-of-the-opera:016"
      "Hidden Parlor"
      [Underground]
      Hourglass
      [Plus, Star]
      Set.ThePhantomOfTheOpera

organChamber :: CardDef
organChamber =
  victory 1
    $ location
      ":the-phantom-of-the-opera:017"
      "Organ Chamber"
      [Underground]
      Star
      [Plus, Hourglass]
      Set.ThePhantomOfTheOpera

parisSewers :: CardCode -> CardDef
parisSewers cardCode =
  location cardCode "Paris Sewers" [Underground] T [] Set.ThePhantomOfTheOpera

parisSewers_018 :: CardDef
parisSewers_018 = parisSewers ":the-phantom-of-the-opera:018"

parisSewers_019 :: CardDef
parisSewers_019 = parisSewers ":the-phantom-of-the-opera:019"

parisSewers_020 :: CardDef
parisSewers_020 = parisSewers ":the-phantom-of-the-opera:020"

parisSewers_021 :: CardDef
parisSewers_021 = parisSewers ":the-phantom-of-the-opera:021"

parisSewers_022 :: CardDef
parisSewers_022 = parisSewers ":the-phantom-of-the-opera:022"

thePhantomsLair :: CardDef
thePhantomsLair =
  ( location
      ":the-phantom-of-the-opera:023"
      "The Phantom's Lair"
      [Underground]
      Plus
      [Hourglass, Star]
      Set.ThePhantomOfTheOpera
  )
    { cdRevealedName = Just ("The Phantom's Lair" <:> "Torture Chamber")
    }
