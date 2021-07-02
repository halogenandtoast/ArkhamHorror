module Arkham.EncounterCard
  ( genEncounterCard
  , lookupEncounterCard
  , lookupEncounterCardDef
  , baseEncounterCard
  , allEncounterCards
  , placeholderEnemy
  , placeholderTreachery
  )
where

import Arkham.Prelude

import Arkham.Enemy.Cards
import Arkham.Location.Cards
import Arkham.Treachery.Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.EncounterSet
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Name
import Arkham.Types.Trait

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

baseEncounterCard
  :: CardType -> CardCode -> Name -> EncounterSet -> CardDef
baseEncounterCard cardType cardCode name encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = cardType
  , cdWeakness = False
  , cdClassSymbol = Nothing
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFast = False
  , cdWindows = mempty
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Just encounterSet
  }

asset :: CardCode -> Name -> EncounterSet -> CardDef
asset = baseEncounterCard EncounterAssetType

allEncounterCards :: HashMap CardCode CardDef
allEncounterCards = allEnemyCards <> allLocationCards <> allEncounterTreacheryCards <>
  mapFromList [ ("02060", jazzMulligan)
  , ("02179", helplessPassenger)
  , ("02215", keyToTheChamber)
  ]

jazzMulligan :: CardDef
jazzMulligan = (asset "02060" ("\"Jazz\" Mulligan" <:> "The Head Janitor") ExtracurricularActivity)
  { cdCardTraits = setFromList [Ally, Miskatonic]
  }

helplessPassenger :: CardDef
helplessPassenger =
  (asset "02179" "Helpless Passenger" TheEssexCountyExpress)
    { cdCardTraits = setFromList [Ally, Bystander]
    , cdKeywords = singleton Keyword.Surge
    }

keyToTheChamber :: CardDef
keyToTheChamber =
  (asset "02215" "Key to the Chamber" BloodOnTheAltar)
    { cdCardTraits = setFromList [Item, Key]
    }
