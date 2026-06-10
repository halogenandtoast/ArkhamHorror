module Arkham.Location.Cards.MistFilledCaverns (mistFilledCaverns) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Stories

newtype MistFilledCaverns = MistFilledCaverns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistFilledCaverns :: LocationCard MistFilledCaverns
mistFilledCaverns = locationWith MistFilledCaverns Cards.mistFilledCaverns 1 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities MistFilledCaverns where
  getAbilities (MistFilledCaverns a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted
        a
        1
        ( Here
            <> thisExists a LocationCanBeFlipped
            <> Negate (Remembered PledForHelp)
            <> Negate (Remembered AffrontedTheRulerOfThisRealm)
        )
        actionAbility

instance RunMessage MistFilledCaverns where
  runMessage msg l@(MistFilledCaverns attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.effigyOfNodens
      pure l
    _ -> MistFilledCaverns <$> liftRunMessage msg attrs
