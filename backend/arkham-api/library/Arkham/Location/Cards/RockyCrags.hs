module Arkham.Location.Cards.RockyCrags (rockyCrags, RockyCrags (..)) where

import Arkham.Ability
import Arkham.Helpers.Window (attachedCard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RockyCrags = RockyCrags LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rockyCrags :: LocationCard RockyCrags
rockyCrags =
  symbolLabel
    $ locationWith RockyCrags Cards.rockyCrags 3 (PerPlayer 2)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities RockyCrags where
  getAbilities (RockyCrags a) =
    extendRevealed1 a
      $ limitedAbility (MaxPer Cards.rockyCrags PerPhase 1)
      $ mkAbility a 1
      $ forced
      $ AttachCard #when Nothing #treachery (targetIs a)

instance RunMessage RockyCrags where
  runMessage msg l@(RockyCrags attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (attachedCard -> card) _ -> do
      push $ GainSurge (attrs.ability 1) (toTarget card)
      pure l
    _ -> RockyCrags <$> liftRunMessage msg attrs
