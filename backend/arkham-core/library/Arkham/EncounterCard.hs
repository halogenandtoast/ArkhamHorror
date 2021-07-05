module Arkham.EncounterCard
  ( genEncounterCard
  , lookupEncounterCard
  , lookupEncounterCardDef
  , allEncounterCards
  , placeholderEnemy
  , placeholderTreachery
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards
import Arkham.Enemy.Cards
import Arkham.Location.Cards
import Arkham.Treachery.Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id

genEncounterCard :: MonadRandom m => CardDef -> m EncounterCard
genEncounterCard cardDef = lookupEncounterCard cardDef <$> getRandom

lookupEncounterCard :: CardDef -> CardId -> EncounterCard
lookupEncounterCard cardDef cardId =
  MkEncounterCard { ecId = cardId, ecDef = cardDef }

lookupEncounterCardDef :: CardCode -> CardDef
lookupEncounterCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

allEncounterCards :: HashMap CardCode CardDef
allEncounterCards =
  allEncounterEnemyCards
    <> allLocationCards
    <> allEncounterTreacheryCards
    <> allEncounterAssetCards
