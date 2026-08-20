module Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheVale where

import Arkham.Location.CardDefs.Import

boardingHouseDay :: CardDef
boardingHouseDay =
  otherSideIs "10705b"
    $ location "10705a" "Boarding House" [HemlockVale] Circle [Diamond, Spade] TheVale

boardingHouseNight :: CardDef
boardingHouseNight =
  otherSideIs "10705a"
    $ location "10705b" "Boarding House" [HemlockVale] Circle [Diamond, Spade] TheVale

hemlockChapelDay :: CardDef
hemlockChapelDay =
  otherSideIs "10707b"
    $ location "10707a" "Hemlock Chapel" [HemlockVale] Triangle [Diamond, Moon] TheVale

hemlockChapelNight :: CardDef
hemlockChapelNight =
  otherSideIs "10707a"
    $ location "10707b" "Hemlock Chapel" [HemlockVale] Triangle [Diamond, Moon] TheVale

tadsGeneralStoreDay :: CardDef
tadsGeneralStoreDay =
  otherSideIs "10710b"
    $ location "10710a" "Tad's General Store" [HemlockVale] Square [Diamond, Star] TheVale

tadsGeneralStoreNight :: CardDef
tadsGeneralStoreNight =
  otherSideIs "10710a"
    $ location "10710b" "Tad's General Store" [HemlockVale] Square [Diamond, Star] TheVale

theAtwoodHouseDay :: CardDef
theAtwoodHouseDay =
  otherSideIs "10709b"
    $ location "10709a" "The Atwood House" [HemlockVale] Moon [Diamond, Triangle, Heart] TheVale

theAtwoodHouseNight :: CardDef
theAtwoodHouseNight =
  otherSideIs "10709a"
    $ location "10709b" "The Atwood House" [HemlockVale] Moon [Diamond, Triangle, Heart, Droplet] TheVale

theCommonsDay :: CardDef
theCommonsDay =
  otherSideIs "10712b"
    $ location "10712a" "The Commons" [HemlockVale] Star [Diamond, Square, Spade] TheVale

theCommonsNight :: CardDef
theCommonsNight =
  otherSideIs "10712a"
    $ location "10712b" "The Commons" [HemlockVale] Star [Diamond, Square, Spade] TheVale

theCrossroadsDay :: CardDef
theCrossroadsDay =
  otherSideIs "10706b"
    $ location
      "10706a"
      "The Crossroads"
      [HemlockVale, Central]
      Diamond
      [Triangle, Square, Star, Circle, Heart, Moon]
      TheVale

theCrossroadsNight :: CardDef
theCrossroadsNight =
  otherSideIs "10706a"
    $ location
      "10706b"
      "The Crossroads"
      [HemlockVale, Central]
      Diamond
      [Triangle, Square, Star, Circle, Heart, Moon]
      TheVale

theOldMillDay :: CardDef
theOldMillDay =
  otherSideIs "10708b"
    $ location "10708a" "The Old Mill" [HemlockVale] Heart [Diamond, Moon] TheVale

theOldMillNight :: CardDef
theOldMillNight =
  otherSideIs "10708a"
    $ location "10708b" "The Old Mill" [HemlockVale] Heart [Diamond, Moon] TheVale

valeSchoolhouseDay :: CardDef
valeSchoolhouseDay =
  otherSideIs "10711b"
    $ location "10711a" "Vale Schoolhouse" [HemlockVale] Spade [Star, Circle] TheVale

valeSchoolhouseNight :: CardDef
valeSchoolhouseNight =
  otherSideIs "10711a"
    $ location "10711b" "Vale Schoolhouse" [HemlockVale] Spade [Star, Circle] TheVale
