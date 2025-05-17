module Arkham.Location.Cards.GreenRoom (greenRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Modifier
import Arkham.Scenarios.CurtainCall.Helpers

newtype GreenRoom = GreenRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greenRoom :: LocationCard GreenRoom
greenRoom = location GreenRoom Cards.greenRoom 5 (PerPlayer 1)

instance HasAbilities GreenRoom where
  getAbilities (GreenRoom a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "greenRoom.investigate"
      $ investigateAbility a 1 mempty Here

instance RunMessage GreenRoom where
  runMessage msg l@(GreenRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #intellect 3)
      investigate sid iid source
      push $ DiscardHand iid source
      pure l
    _ -> GreenRoom <$> liftRunMessage msg attrs
