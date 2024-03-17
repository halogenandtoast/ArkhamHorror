module Arkham.Location.Cards.PeaksOfThok (peaksOfThok, PeaksOfThok (..)) where

import Arkham.Ability
import Arkham.Helpers.Message (assignDamage)
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Source
import Arkham.Story.Cards qualified as Story
import Arkham.Target

newtype PeaksOfThok = PeaksOfThok LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peaksOfThok :: LocationCard PeaksOfThok
peaksOfThok = location PeaksOfThok Cards.peaksOfThok 3 (Static 0)

instance HasAbilities PeaksOfThok where
  getAbilities (PeaksOfThok attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 Here actionAbility
      , mkAbility attrs 2 $ forced $ Leaves #after You (be attrs)
      ]

instance RunMessage PeaksOfThok where
  runMessage msg l@(PeaksOfThok attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginSkillTest iid (attrs.ability 1) iid #agility 5
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (locationCanBeFlipped attrs)
        $ flipOver iid attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      beginSkillTest iid (attrs.ability 2) iid #agility 2
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ assignDamage iid (toAbilitySource attrs 2) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.inhabitantsOfTheVale
      pure . PeaksOfThok $ attrs & canBeFlippedL .~ False
    _ -> PeaksOfThok <$> lift (runMessage msg attrs)
