{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Enemy
  ( module Arkham.Types.Enemy
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Cards
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait (Trait, toTraits)
import Arkham.Types.TreacheryId

$(buildEntity "Enemy")

createEnemy :: IsCard a => a -> Enemy
createEnemy a = lookupEnemy (toCardCode a) (EnemyId $ toCardId a)

instance HasCardDef Enemy where
  toCardDef = toCardDef . toAttrs

instance HasName env Enemy where
  getName = getName . toAttrs

actionFromMessage :: Message -> Maybe Action
actionFromMessage (UseAbility _ ability) = case abilityType ability of
  ActionAbility maction _ -> maction
  _ -> Nothing
actionFromMessage _ = Nothing

preventedByModifier :: EnemyAttrs -> Message -> ModifierType -> Bool
preventedByModifier e msg (CannotTakeAction matcher) =
  case actionFromMessage msg of
    Just action -> case matcher of
      IsAction a -> a == action
      EnemyAction a traits -> a == action && notNull
        (setFromList traits `intersect` toTraits (toCardDef e))
      FirstOneOf _ -> False -- TODO: We can't tell here
    Nothing -> False
preventedByModifier _ _ _ = False

instance ActionRunner env => HasActions env Enemy where
  getActions investigator window x = do
    modifiers' <- getModifiers (toSource x) (InvestigatorTarget investigator)
    actions <- defaultGetActions investigator window x
    pure $ filter
      (\action -> not $ any (preventedByModifier (toAttrs x) action) modifiers')
      actions

instance
    ( HasId LocationId env InvestigatorId
    , HasCount RemainingSanity env InvestigatorId
    , HasCount CardCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasSet InvestigatorId env LocationId
    , HasSet ConnectedLocationId env LocationId
    , HasSet Trait env AssetId
    , HasSet Trait env LocationId
    , HasSet LocationId env ()
    , HasModifiersFor env ()
    , HasName env AssetId
    , HasId (Maybe LocationId) env AssetId
    , HasSkillValue env InvestigatorId
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

instance
    ( EnemyRunner env
    , HasName env AssetId
    , HasSet ClosestLocationId env (LocationId, [Trait])
    , HasCount DiscardCount env InvestigatorId
    , HasSet EnemyId env ()
    , HasId (Maybe StoryEnemyId) env CardCode
    ) =>
    RunMessage env Enemy
    where
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

instance HasCount DoomCount env Enemy where
  getCount = pure . DoomCount . enemyDoom . toAttrs

instance HasCount ClueCount env Enemy where
  getCount = pure . ClueCount . enemyClues . toAttrs

instance HasCount FightCount env Enemy where
  getCount = pure . FightCount . enemyFight . toAttrs

instance HasCount HealthDamageCount env Enemy where
  getCount = pure . HealthDamageCount . enemyHealthDamage . toAttrs

instance HasCount SanityDamageCount env Enemy where
  getCount = pure . SanityDamageCount . enemySanityDamage . toAttrs

instance HasId LocationId env Enemy where
  getId = pure . enemyLocation . toAttrs

instance HasSet TreacheryId env Enemy where
  getSet = pure . enemyTreacheries . toAttrs

instance HasSet AssetId env Enemy where
  getSet = pure . enemyAssets . toAttrs

instance IsCard Enemy where
  toCardId = toCardId . toAttrs

instance HasDamage Enemy where
  getDamage = (, 0) . enemyDamage . toAttrs

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy cardCode =
  fromJustNote ("Unknown enemy: " <> pack (show cardCode))
    $ lookup cardCode allEnemies

allEnemies :: Map CardCode (EnemyId -> Enemy)
allEnemies = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Enemy")

isEngaged :: Enemy -> Bool
isEngaged = notNull . enemyEngagedInvestigators . toAttrs

isUnique :: Enemy -> Bool
isUnique = cdUnique . toCardDef

instance Exhaustable Enemy where
  isExhausted = enemyExhausted . toAttrs

getEngagedInvestigators :: Enemy -> Set InvestigatorId
getEngagedInvestigators = enemyEngagedInvestigators . toAttrs

getEnemyVictory :: Enemy -> Maybe Int
getEnemyVictory = cdVictoryPoints . toCardDef

getBearer :: Enemy -> Maybe InvestigatorId
getBearer x = case enemyPrey (toAttrs x) of
  Bearer iid -> Just (unBearerId iid)
  _ -> Nothing
