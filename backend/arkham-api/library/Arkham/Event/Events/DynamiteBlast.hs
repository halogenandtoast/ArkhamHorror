module Arkham.Event.Events.DynamiteBlast where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), withoutModifier)
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype DynamiteBlast = DynamiteBlast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast :: EventCard DynamiteBlast
dynamiteBlast = event DynamiteBlast Cards.dynamiteBlast

instance RunMessage DynamiteBlast where
  runMessage msg e@(DynamiteBlast attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <- select $ orConnected (locationWithInvestigator iid)
      canDealDamage <- withoutModifier iid CannotDealDamage
      chooseOneM iid do
        for_ locations \location -> do
          enemies <- if canDealDamage then select (enemyAt location) else pure []
          investigators <- select $ investigatorAt location
          unless (null enemies && null investigators) do
            targeting location do
              uiEffect attrs location Explosion
              for_ enemies (nonAttackEnemyDamage attrs 3)
              for_ investigators \iid' -> assignDamage iid' attrs 3
      pure e
    _ -> DynamiteBlast <$> liftRunMessage msg attrs
