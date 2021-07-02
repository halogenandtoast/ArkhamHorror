module Arkham.EncounterCard
  ( genEncounterCard
  , lookupEncounterCard
  , lookupEncounterCardDef
  , allEncounterCards
  , placeholderEnemy
  , placeholderTreachery
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards
import Arkham.Enemy.Cards
import Arkham.Location.Cards
import Arkham.Treachery.Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id

genEncounterCard :: MonadRandom m => CardCode -> m EncounterCard
genEncounterCard cardCode = lookupEncounterCard cardCode <$> getRandom

lookupEncounterCard :: CardCode -> CardId -> EncounterCard
lookupEncounterCard cardCode cardId = MkEncounterCard
  { ecId = cardId
  , ecDef = lookupEncounterCardDef cardCode
  }

lookupEncounterCardDef :: CardCode -> CardDef
lookupEncounterCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

allEncounterCards :: HashMap CardCode CardDef
allEncounterCards = allEncounterEnemyCards
  <> allLocationCards
  <> allEncounterTreacheryCards
  <> allEncounterAssetCards
