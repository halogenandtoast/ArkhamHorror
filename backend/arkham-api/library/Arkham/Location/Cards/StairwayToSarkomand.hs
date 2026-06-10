module Arkham.Location.Cards.StairwayToSarkomand (stairwayToSarkomand) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Stories

newtype StairwayToSarkomand = StairwayToSarkomand LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stairwayToSarkomand :: LocationCard StairwayToSarkomand
stairwayToSarkomand = locationWith StairwayToSarkomand Cards.stairwayToSarkomand 3 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities StairwayToSarkomand where
  getAbilities (StairwayToSarkomand a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted
        a
        1
        ( Here
            <> thisExists a LocationCanBeFlipped
            <> Negate (Remembered WarnedTheDenizensOfSarkomand)
            <> Negate (Remembered CutOffAllEscape)
        )
        actionAbility

instance RunMessage StairwayToSarkomand where
  runMessage msg l@(StairwayToSarkomand attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.ruinsOfSarkomand
      pure l
    _ -> StairwayToSarkomand <$> liftRunMessage msg attrs
