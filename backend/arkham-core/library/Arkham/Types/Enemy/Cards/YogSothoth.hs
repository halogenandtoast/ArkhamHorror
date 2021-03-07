module Arkham.Types.Enemy.Cards.YogSothoth
  ( yogSothoth
  , YogSothoth(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Modifier
import Arkham.Types.Query

newtype YogSothoth = YogSothoth EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothoth :: EnemyId -> YogSothoth
yogSothoth uuid =
  YogSothoth
    $ baseAttrs uuid "02323"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 5)
    . (fightL .~ 4)
    . (healthL .~ Static 4)
    . (evadeL .~ 0)
    . (uniqueL .~ True)

instance HasCount PlayerCount env () => HasModifiersFor env YogSothoth where
  getModifiersFor _ target (YogSothoth a@EnemyAttrs {..}) | isTarget a target =
    do
      healthModifier <- getPlayerCountValue (PerPlayer 6)
      pure $ toModifiers
        a
        [ HealthModifier healthModifier
        , CannotMakeAttacksOfOpportunity
        , CannotBeEvaded
        ]
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasActions env => HasActions env YogSothoth where
  getActions i window (YogSothoth attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env YogSothoth where
  runMessage msg (YogSothoth attrs) = YogSothoth <$> runMessage msg attrs
