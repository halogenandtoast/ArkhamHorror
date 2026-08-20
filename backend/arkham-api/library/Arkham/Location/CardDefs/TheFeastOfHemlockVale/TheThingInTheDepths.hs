module Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheThingInTheDepths where

import Arkham.Location.CardDefs.Import

abandonedShack :: CardDef
abandonedShack =
  otherSideIs "10597b"
    $ location_ "10597a" "Abandoned Shack" [Bog] TheThingInTheDepths

coveredBridge :: CardDef
coveredBridge =
  victory 1
    $ otherSideIs "10599b"
    $ location_ "10599a" "Covered Bridge" [Bog] TheThingInTheDepths

fetidPool :: CardDef
fetidPool =
  quantity 2
    $ otherSideIs "10595b"
    $ location_ "10595a" "Fetid Pool" [Bog] TheThingInTheDepths

floodedPath :: CardDef
floodedPath =
  quantity 2
    $ otherSideIs "10596b"
    $ location_ "10596a" "Flooded Path" [Bog] TheThingInTheDepths

muddyFen :: CardDef
muddyFen =
  quantity 2
    $ victory 1
    $ otherSideIs "10593b"
    $ location_ "10593a" "Muddy Fen" [Bog] TheThingInTheDepths

openWater10593b :: CardDef
openWater10593b =
  quantity 2
    $ victory 1
    $ otherSideIs "10593a"
    $ location_ "10593b" "Open Water" [Sunken] TheThingInTheDepths

openWater10594b :: CardDef
openWater10594b =
  otherSideIs "10594a"
    $ location_ "10594b" "Open Water" [Sunken] TheThingInTheDepths

openWater10595b :: CardDef
openWater10595b =
  quantity 2
    $ otherSideIs "10595a"
    $ location_ "10595b" "Open Water" [Sunken] TheThingInTheDepths

openWater10596b :: CardDef
openWater10596b =
  quantity 2
    $ otherSideIs "10596a"
    $ location_ "10596b" "Open Water" [Sunken] TheThingInTheDepths

openWater10597b :: CardDef
openWater10597b =
  otherSideIs "10597a"
    $ location_ "10597b" "Open Water" [Sunken] TheThingInTheDepths

openWater10598b :: CardDef
openWater10598b =
  otherSideIs "10598a"
    $ location_ "10598b" "Open Water" [Sunken] TheThingInTheDepths

openWater10599b :: CardDef
openWater10599b =
  victory 1
    $ otherSideIs "10599a"
    $ location_ "10599b" "Open Water" [Sunken] TheThingInTheDepths

rottenDock :: CardDef
rottenDock =
  otherSideIs "10598b"
    $ location_ "10598a" "Rotten Dock" [Bog] TheThingInTheDepths

tangledThicket :: CardDef
tangledThicket =
  otherSideIs "10594b"
    $ location_ "10594a" "Tangled Thicket" [Bog] TheThingInTheDepths
