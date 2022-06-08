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
import Arkham.Trait (toTraits)
import Data.Aeson.TH

$(buildEntity "Enemy")

$(deriveJSON defaultOptions ''Enemy)

createEnemy :: (HasCallStack, IsCard a) => a -> Enemy
createEnemy a = lookupEnemy (toCardCode a) (EnemyId $ toCardId a)

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
  getAbilities = $(entityF "Enemy" "getAbilities")

instance HasModifiersFor Enemy where
  getModifiersFor = $(entityF2 "Enemy" "getModifiersFor")

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Enemy" "toAttrs")

instance TargetEntity Enemy where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance RunMessage Enemy where
  runMessage msg e = do
    -- we must check that an enemy exists when grabbing modifiers
    -- as some messages are not masked when targetting cards in the
    -- discard.
    allEnemyIds <- select AnyEnemy
    modifiers' <- if toId e `member` allEnemyIds
      then getModifiers (toSource e) (toTarget e)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    $(entityRunMessage "Enemy") msg' e

lookupEnemy :: HasCallStack => CardCode -> (EnemyId -> Enemy)
lookupEnemy cardCode =
  fromJustNote ("Unknown enemy: " <> pack (show cardCode))
    $ lookup cardCode allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Enemy")
