module Arkham.Location.Cards.Sewer (sewer) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers

newtype Sewer = Sewer LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sewer :: LocationCard Sewer
sewer = locationWith Sewer Cards.sewer 5 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Sewer where
  getAbilities (Sewer a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "sewer.move"
      $ skillTestAbility
      $ restricted a 1 Here actionAbility

instance RunMessage Sewer where
  runMessage msg l@(Sewer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 1)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) succeededBy -> do
      locations <-
        select
          $ LocationWithDistanceFromAtMost succeededBy (be attrs) (not_ (be attrs))
      chooseTargetM iid locations \chosen -> moveTo (attrs.ability 1) iid chosen
      pure l
    _ -> Sewer <$> liftRunMessage msg attrs
