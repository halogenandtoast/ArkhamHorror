module Arkham.Location.CardDefs.GuardiansOfTheAbyss.SandsOfEgypt where

import Arkham.Location.CardDefs.Import

desertOasis :: CardDef
desertOasis =
  singleSided
    $ location
      "83043"
      "Desert Oasis"
      [Expedition, Desert]
      Heart
      [Star, Moon, Hourglass, Squiggle]
      SandsOfEgypt

dunesOfTheSahara :: CardDef
dunesOfTheSahara =
  singleSided
    $ location
      "83040"
      "Dunes of the Sahara"
      [Expedition, Desert]
      Plus
      [Circle, Hourglass, T, Droplet]
      SandsOfEgypt

expeditionCamp :: CardDef
expeditionCamp =
  singleSided
    $ location
      "83037"
      "Expedition Camp"
      [Expedition, Cairo, Desert]
      Circle
      [Diamond, Hourglass, Moon, Plus]
      SandsOfEgypt

facelessSphinx :: CardDef
facelessSphinx =
  singleSided
    $ location "83042" "Faceless Sphinx" [Expedition, Desert, Ruins] Star [Moon, Heart] SandsOfEgypt

nileRiver :: CardDef
nileRiver =
  singleSided
    $ location
      "83038"
      "Nile River"
      [Expedition, Desert]
      Moon
      [Circle, Star, Heart, Hourglass]
      SandsOfEgypt

sandsOfDashur :: CardDef
sandsOfDashur =
  singleSided
    $ location
      "83039"
      "Sands of Dashur"
      [Expedition, Desert]
      Hourglass
      [Circle, Plus, T, Squiggle, Heart, Moon]
      SandsOfEgypt

sandsweptRuins :: CardDef
sandsweptRuins =
  singleSided
    $ location
      "83044"
      "Sandswept Ruins"
      [Expedition, Desert, Ruins]
      T
      [Droplet, Plus, Hourglass, Squiggle]
      SandsOfEgypt

untouchedVault :: CardDef
untouchedVault =
  singleSided
    $ location "83041" "Untouched Vault" [Expedition, Ruins] Droplet [Plus, T] SandsOfEgypt
