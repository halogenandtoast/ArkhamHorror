{-# LANGUAGE TemplateHaskell #-}
module Arkham.Enemy
  ( module Arkham.Enemy
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Enemies
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Name
import Arkham.Query
import Arkham.Trait (Trait, toTraits)

$(buildEntity "Enemy")

createEnemy :: (HasCallStack, IsCard a) => a -> Enemy
createEnemy a = lookupEnemy (toCardCode a) (EnemyId $ toCardId a)

instance HasCardDef Enemy where
  toCardDef = toCardDef . toAttrs

instance HasName env Enemy where
  getName = getName . toAttrs

actionFromMessage :: Ability -> Maybe Action
actionFromMessage ability = case abilityType ability of
  ActionAbility maction _ -> maction
  _ -> Nothing

preventedByModifier :: EnemyAttrs -> Ability -> ModifierType -> Bool
preventedByModifier e msg (CannotTakeAction matcher) =
  case actionFromMessage msg of
    Just action -> case matcher of
      IsAction a -> a == action
      EnemyAction a traits -> a == action && notNull
        (setFromList traits `intersect` toTraits (toCardDef e))
      FirstOneOf _ -> False -- TODO: We can't tell here
    Nothing -> False
preventedByModifier _ _ _ = False

instance HasAbilities Enemy where
  getAbilities = genericGetAbilities

instance
    ( HasId LocationId env InvestigatorId
    , HasCount RemainingSanity env InvestigatorId
    , HasCount CardCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasCount Shroud env LocationId
    , HasSet InvestigatorId env LocationId
    , HasSet ConnectedLocationId env LocationId
    , HasSet Trait env AssetId
    , HasSet LocationId env ()
    , HasModifiersFor env ()
    , HasName env AssetId
    , HasPhase env
    , HasStep AgendaStep env ()
    , HasId (Maybe LocationId) env AssetId
    , HasSkillValue env InvestigatorId
    , Query LocationMatcher env
    ) =>
    HasModifiersFor env Enemy
    where
  getModifiersFor = genericGetModifiersFor

instance HasCardCode Enemy where
  toCardCode = toCardCode . toAttrs

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs

instance Named Enemy where
  toName = toName . toAttrs

instance TargetEntity Enemy where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance EnemyRunner env => RunMessage env Enemy where
  runMessage msg e = do
    -- we must check that an enemy exists when grabbing modifiers
    -- as some messages are not masked when targetting cards in the
    -- discard.
    allEnemyIds <- getSet @EnemyId ()
    modifiers' <- if toId e `member` allEnemyIds
      then getModifiers (toSource e) (toTarget e)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    genericRunMessage msg' e

instance HasVictoryPoints Enemy where
  getVictoryPoints = getEnemyVictory

instance HasModifiersFor env () => HasCount DoomCount env Enemy where
  getCount e = do
    modifiers <- getModifiers (toSource e) (toTarget e)
    let f = if DoomSubtracts `elem` modifiers then negate else id
    pure . DoomCount . f . enemyDoom $ toAttrs e

instance HasCount ClueCount env Enemy where
  getCount = pure . ClueCount . enemyClues . toAttrs

instance HasCount FightCount env Enemy where
  getCount = pure . FightCount . enemyFight . toAttrs

instance HasCount HealthDamageCount env Enemy where
  getCount = pure . HealthDamageCount . enemyHealthDamage . toAttrs

instance HasCount SanityDamageCount env Enemy where
  getCount = pure . SanityDamageCount . enemySanityDamage . toAttrs

instance HasId (Maybe LocationId) env Enemy where
  getId = pure . enemyLocation . toAttrs

instance HasSet TreacheryId env Enemy where
  getSet = pure . enemyTreacheries . toAttrs

instance HasSet AssetId env Enemy where
  getSet = pure . enemyAssets . toAttrs

instance IsCard Enemy where
  toCardId = toCardId . toAttrs
  toCardOwner = toCardOwner . toAttrs

instance HasDamage Enemy where
  getDamage = (, 0) . enemyDamage . toAttrs

lookupEnemy :: HasCallStack => CardCode -> (EnemyId -> Enemy)
lookupEnemy cardCode =
  fromJustNote ("Unknown enemy: " <> pack (show cardCode))
    $ lookup cardCode allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Enemy")

isEngaged :: Enemy -> Bool
isEngaged = notNull . enemyEngagedInvestigators . toAttrs

isUnique :: Enemy -> Bool
isUnique = cdUnique . toCardDef

remainingHealth
  :: (HasCount PlayerCount env (), MonadReader env m) => Enemy -> m Int
remainingHealth e = do
  totalHealth <- getPlayerCountValue (enemyHealth attrs)
  pure $ totalHealth - enemyDamage attrs
  where attrs = toAttrs e

instance Exhaustable Enemy where
  isExhausted = enemyExhausted . toAttrs

getEngagedInvestigators :: Enemy -> HashSet InvestigatorId
getEngagedInvestigators = enemyEngagedInvestigators . toAttrs

getEnemyVictory :: Enemy -> Maybe Int
getEnemyVictory = cdVictoryPoints . toCardDef

getEnemyBearer :: Enemy -> Maybe InvestigatorId
getEnemyBearer = enemyBearer . toAttrs

getEnemyLocation :: Enemy -> Maybe LocationId
getEnemyLocation = enemyLocation . toAttrs
