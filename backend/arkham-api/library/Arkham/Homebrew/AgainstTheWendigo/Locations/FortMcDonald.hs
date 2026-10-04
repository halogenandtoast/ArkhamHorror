module Arkham.Homebrew.AgainstTheWendigo.Locations.FortMcDonald (fortMcDonald) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (civilizedResign, scenarioI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype FortMcDonald = FortMcDonald LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortMcDonald :: LocationCard FortMcDonald
fortMcDonald = location FortMcDonald Cards.fortMcDonald 3 (PerPlayer 1)

instance HasAbilities FortMcDonald where
  getAbilities (FortMcDonald a) =
    extendRevealed
      a
      [ civilizedResign a
      , -- "Collective limit of 1 [per_investigator] clue per game", i.e. each
        -- investigator can win the fort one clue, once.
        playerLimit PerGame $ skillTestAbility $ restricted a 1 Here parleyAction_
      , playerLimit PerGame
          $ restricted a 2 Here
          $ actionAbilityWithCost (ResourceCost 2)
      ]


instance RunMessage FortMcDonald where
  runMessage msg l@(FortMcDonald attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainClues iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseOneM iid $ scenarioI18n $ scope "fortMcDonald" do
        labeled "healDamage" $ healDamage iid (attrs.ability 2) 2
        labeled "healHorror" $ healHorror iid (attrs.ability 2) 2
      pure l
    _ -> FortMcDonald <$> liftRunMessage msg attrs
