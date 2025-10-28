module Arkham.Campaigns.TheScarletKeys.Key.Cards where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Prelude

key :: CardCode -> Name -> EncounterSet -> CardDef
key cardCode name encounterSet =
  (emptyCardDef cardCode name KeyType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = True
    , cdOtherSide = Just $ flippedCardCode cardCode
    , cdLevel = Nothing
    }

allScarletKeyCards :: Map CardCode CardDef
allScarletKeyCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [ theEyeOfRavens
      , theLastBlossom
      , theWeepingLady
      , theTwistedAntiprism
      , theSableGlass
      , theLightOfPharos
      , theShadeReaper
      , theMirroringBlade
      , theBaleEngine
      , theRuinousChime
      , theWellspringOfFortune
      ]

theEyeOfRavens :: CardDef
theEyeOfRavens = key "09519" ("The Eye of Ravens" <:> "Macabre Ruby Marble") RiddlesAndRain

theLastBlossom :: CardDef
theLastBlossom = key "09544" ("The Last Blossom" <:> "Paracausal Symbiote") DeadHeat

theWeepingLady :: CardDef
theWeepingLady = key "09565" ("The Weeping Lady" <:> "Broken Statuette") SanguineShadows

theTwistedAntiprism :: CardDef
theTwistedAntiprism = key "09590" ("The Twisted Antiprism" <:> "Talisman of Infinite Darkness") DealingsInTheDark

theSableGlass :: CardDef
theSableGlass = key "09634" ("The Sable Glass" <:> "Translocated Lens") OnThinIce

theLightOfPharos :: CardDef
theLightOfPharos = key "09659" ("The Light of Pharos" <:> "Coral Prism") DogsOfWar

theShadeReaper :: CardDef
theShadeReaper = key "09680" ("The Shade Reaper" <:> "Harvester of Woe") ShadesOfSuffering

theMirroringBlade :: CardDef
theMirroringBlade = key "09768" ("The Mirroring Blade" <:> "Reflects Only Violence") Globetrotting

theBaleEngine :: CardDef
theBaleEngine = key "09769" ("The Bale Engine" <:> "Infernal Mechanism") Globetrotting

theRuinousChime :: CardDef
theRuinousChime = key "09770" ("The Ruinous Chime" <:> "Silent Cacophony") Globetrotting

theWellspringOfFortune :: CardDef
theWellspringOfFortune = key "88045" ("The Wellspring of Fortune" <:> "88045") FortuneAndFolly
