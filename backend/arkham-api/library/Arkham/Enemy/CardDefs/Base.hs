module Arkham.Enemy.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding (Blight, Byakhee, Dreamlands)
import Arkham.GameValue
import Arkham.Name
import Arkham.Prelude

baseEnemy
  :: CardCode
  -> Name
  -> Maybe (EncounterSet, Int)
  -> Maybe CardSubType
  -> CardDef
baseEnemy cardCode name mEncounterSet isWeakness =
  (emptyCardDef cardCode name $ if isJust isWeakness then PlayerEnemyType else EnemyType)
    { cdCardSubType = isWeakness
    , cdClassSymbols = if isJust isWeakness then singleton Neutral else mempty
    , cdEncounterSet = fst <$> mEncounterSet
    , cdEncounterSetQuantity = snd <$> mEncounterSet
    , cdLevel = Nothing
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

doubleSided :: CardCode -> CardDef -> CardDef
doubleSided cCode def =
  def
    { cdDoubleSided = True
    , cdOtherSide = Just cCode
    }

-- | Printed-health helpers for the @cdHealth@ field, e.g. @cdHealth = health 1@.
health :: Int -> Maybe Health
health = Just . Health . Static

healthPerInvestigator :: Int -> Maybe Health
healthPerInvestigator = Just . Health . PerPlayer

healthX :: Maybe Health
healthX = Just (Health ValueX)

healthStar :: Maybe Health
healthStar = Just (Health ValueStar)

-- | Printed-fight helpers for the @cdFight@ field, e.g. @cdFight = fight 3@.
fight :: Int -> Maybe Fight
fight = Just . FightValue . Static

fightPerInvestigator :: Int -> Maybe Fight
fightPerInvestigator = Just . FightValue . PerPlayer

fightX :: Maybe Fight
fightX = Just (FightValue ValueX)

fightStar :: Maybe Fight
fightStar = Just (FightValue ValueStar)

fightUnknown :: Maybe Fight
fightUnknown = Just (FightValue ValueUnknown)

-- | Printed-evade helpers for the @cdEvade@ field, e.g. @cdEvade = evade 2@.
evade :: Int -> Maybe Evade
evade = Just . EvadeValue . Static

evadePerInvestigator :: Int -> Maybe Evade
evadePerInvestigator = Just . EvadeValue . PerPlayer

evadeX :: Maybe Evade
evadeX = Just (EvadeValue ValueX)

evadeStar :: Maybe Evade
evadeStar = Just (EvadeValue ValueStar)

evadeUnknown :: Maybe Evade
evadeUnknown = Just (EvadeValue ValueUnknown)

-- | Printed health-damage helper for @cdHealthDamage@, e.g. @cdHealthDamage = healthDamage 1@.
healthDamage :: Int -> Maybe HealthDamage
healthDamage = Just . HealthDamageValue . Static

-- | Printed sanity-damage helper for @cdSanityDamage@, e.g. @cdSanityDamage = sanityDamage 1@.
sanityDamage :: Int -> Maybe SanityDamage
sanityDamage = Just . SanityDamageValue . Static

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseEnemy cardCode name Nothing (Just Weakness)

basicWeakness :: CardCode -> Name -> CardDef
basicWeakness cardCode name =
  baseEnemy cardCode name Nothing (Just BasicWeakness)

enemy :: CardCode -> Name -> EncounterSet -> Int -> CardDef
enemy cardCode name encounterSet encounterSetQuantity =
  baseEnemy cardCode name (Just (encounterSet, encounterSetQuantity)) Nothing
