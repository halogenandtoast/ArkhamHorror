module Arkham.Event.Events.MakeshiftBomb3 (makeshiftBomb3) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher

newtype MakeshiftBomb3 = MakeshiftBomb3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

makeshiftBomb3 :: EventCard MakeshiftBomb3
makeshiftBomb3 = event MakeshiftBomb3 Cards.makeshiftBomb3

instance HasAbilities MakeshiftBomb3 where
  getAbilities (MakeshiftBomb3 a) = case a.attachedTo.location of
    Just lid ->
      [ controlled_ a 1
          $ triggered
            ( oneOf
                [EnemyEnters #after (LocationWithId lid) AnyEnemy, Enters #after Anyone (LocationWithId lid)]
            )
            (discardCost a)
      ]
    _ -> []

instance RunMessage MakeshiftBomb3 where
  runMessage msg e@(MakeshiftBomb3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo.location \location -> do
        enemies <- select $ enemyAt location
        investigators <- select $ investigatorAt location
        for_ enemies $ nonAttackEnemyDamage (Just iid) attrs 3
        for_ investigators \iid' -> assignDamage iid' attrs 3
      pure e
    _ -> MakeshiftBomb3 <$> liftRunMessage msg attrs
