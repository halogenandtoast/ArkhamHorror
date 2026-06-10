module Arkham.Location.Cards.TunnelsUnderNgranek (tunnelsUnderNgranek) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Stories

newtype TunnelsUnderNgranek = TunnelsUnderNgranek LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tunnelsUnderNgranek :: LocationCard TunnelsUnderNgranek
tunnelsUnderNgranek = locationWith TunnelsUnderNgranek Cards.tunnelsUnderNgranek 2 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities TunnelsUnderNgranek where
  getAbilities (TunnelsUnderNgranek a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted
        a
        1
        ( Here
            <> thisExists a LocationCanBeFlipped
            <> Negate (Remembered FreedTheNightgaunts)
            <> Negate (Remembered ExecutedTheNightgaunts)
        )
        actionAbility

instance RunMessage TunnelsUnderNgranek where
  runMessage msg l@(TunnelsUnderNgranek attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.prisonersOfConquest
      pure l
    _ -> TunnelsUnderNgranek <$> liftRunMessage msg attrs
