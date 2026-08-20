module Arkham.Story.CardDefs.MachinationsThroughTime where

import Arkham.Story.CardDefs.Import

aBitterRivalry :: CardDef
aBitterRivalry =
  doubleSided $ addTrait Machination $ story "87033" "A Bitter Rivalry" MachinationsThroughTime

aNobleLegacyFuture :: CardDef
aNobleLegacyFuture = doubleSided $ story "87024" "A Noble Legacy (Future)" MachinationsThroughTime

aNobleLegacyPast :: CardDef
aNobleLegacyPast = doubleSided $ story "87006" "A Noble Legacy (Past)" MachinationsThroughTime

aNobleLegacyPresent :: CardDef
aNobleLegacyPresent = doubleSided $ story "87015" "A Noble Legacy (Present)" MachinationsThroughTime

anomaliesInSpacetime :: CardDef
anomaliesInSpacetime =
  doubleSided $ addTrait Plot $ story "87038" "Anomalies in Spacetime" MachinationsThroughTime

mobTroubles :: CardDef
mobTroubles = doubleSided $ addTrait Plot $ story "87039" "Mob Troubles" MachinationsThroughTime

redeemAFormerColleague :: CardDef
redeemAFormerColleague =
  doubleSided
    $ addTrait Machination
    $ story "87034" "Redeem a Former Colleague" MachinationsThroughTime

uneasyAlliance :: CardDef
uneasyAlliance =
  doubleSided $ addTrait Machination $ story "87035" "Uneasy Alliance" MachinationsThroughTime

unspeakableAbomination :: CardDef
unspeakableAbomination =
  doubleSided $ addTrait Plot $ story "87042" "Unspeakable Abomination" MachinationsThroughTime
