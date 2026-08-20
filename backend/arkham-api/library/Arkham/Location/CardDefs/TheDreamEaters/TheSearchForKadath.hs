module Arkham.Location.CardDefs.TheDreamEaters.TheSearchForKadath where

import Arkham.Location.CardDefs.Import

baharna :: CardDef
baharna =
  victory 1
    $ veiled
    $ location "06136" "Baharna" [Oriab, City, Port] Circle [Square, Diamond] TheSearchForKadath

celephais :: CardDef
celephais =
  victory 1
    $ veiled
    $ location
      "06139"
      "Celephaïs"
      [OothNargai, City, Port]
      Hourglass
      [Moon, Plus]
      TheSearchForKadath

cityWhichAppearsOnNoMap :: CardDef
cityWhichAppearsOnNoMap =
  victory 2
    $ veiled
    $ location
      "06143"
      "City-Which-Appears-On-No-Map"
      [City, Otherworld]
      Star
      [T]
      TheSearchForKadath

dylathLeen :: CardDef
dylathLeen = veiled $ location "06129" "Dylath-Leen" [Skai, City, Port] Triangle [Squiggle] TheSearchForKadath

forbiddenLands :: CardDef
forbiddenLands =
  singleSided
    $ location "06134" "Forbidden Lands" [Forbidden, Wastes] Diamond [Circle, Square] TheSearchForKadath

hazuthKleg :: CardDef
hazuthKleg =
  veiled
    $ location
      "06141"
      "Hazuth-Kleg"
      [OothNargai, City]
      Plus
      [Hourglass, T]
      TheSearchForKadath

ilekVad :: CardDef
ilekVad =
  victory 1
    $ veiled
    $ location "06133" "Ilek-Vad" [Forbidden, City, Port] Circle [Diamond] TheSearchForKadath

kadatheron :: CardDef
kadatheron =
  veiled
    $ location
      "06130"
      "Kadatheron"
      [Mnar, Ancient, City, Port]
      Circle
      [Square, Diamond]
      TheSearchForKadath

mtNgranek :: CardDef
mtNgranek =
  victory 1
    $ veiled
    $ location "06137" "Mt. Ngranek" [Oriab, Mountain] Square [Circle, Diamond] TheSearchForKadath

namelessRuins :: CardDef
namelessRuins =
  victory 1
    $ veiled
    $ location
      "06138"
      "Nameless Ruins"
      [Oriab, Ancient, Ruins]
      Diamond
      [Circle, Square]
      TheSearchForKadath

ruinsOfIb :: CardDef
ruinsOfIb =
  victory 1
    $ veiled
    $ location "06132" "Ruins of Ib" [Mnar, Ancient, Ruins] Square [Circle, Diamond] TheSearchForKadath

sarnath :: CardDef
sarnath =
  victory 1
    $ veiled
    $ location "06131" "Sarnath" [Mnar, Ancient, Ruins] Diamond [Circle, Square] TheSearchForKadath

serannian :: CardDef
serannian =
  victory 1
    $ veiled
    $ location
      "06140"
      "Serannian"
      [OothNargai, City, Port]
      Moon
      [Hourglass]
      TheSearchForKadath

skaiRiver :: CardDef
skaiRiver =
  singleSided
    $ location "06128" "Skai River" [Skai, City] Squiggle [Heart, Triangle] TheSearchForKadath

templeOfUnattainableDesires :: CardDef
templeOfUnattainableDesires =
  victory 1
    $ veiled
    $ location
      "06142"
      "Temple of Unattainable Desires"
      [OothNargai, Temple]
      T
      [Plus, Star]
      TheSearchForKadath

ulthar :: CardDef
ulthar = veiled $ location "06127" "Ulthar" [Skai, City] Heart [Squiggle] TheSearchForKadath

zulanThek :: CardDef
zulanThek =
  veiled
    $ location "06135" "Zulan-Thek" [Forbidden, City] Square [Diamond] TheSearchForKadath
