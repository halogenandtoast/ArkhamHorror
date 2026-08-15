module Arkham.Homebrew.DarkMatter.Locations.Cafeteria (cafeteria) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scenarioI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Cafeteria = Cafeteria LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cafeteria :: LocationCard Cafeteria
cafeteria = locationWith Cafeteria Cards.cafeteria 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Cafeteria where
  getAbilities (Cafeteria a) =
    extendRevealed1 a
      $ scenarioI18n "electricNightmare"
      $ withI18nTooltip "cafeteria.investigate"
      $ investigateAbility a 1 mempty Here

instance RunMessage Cafeteria where
  runMessage msg l@(Cafeteria attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) sid (Difficulty 2)
      investigate sid iid (attrs.ability 1)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      adjacent <- select $ connectedFrom (be attrs) <> LocationWithAnyClues
      chooseOrRunOneM iid $ targets adjacent $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure l
    _ -> Cafeteria <$> liftRunMessage msg attrs
