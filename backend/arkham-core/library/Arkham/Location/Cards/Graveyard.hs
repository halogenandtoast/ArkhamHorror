module Arkham.Location.Cards.Graveyard where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (graveyard)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement

newtype Graveyard = Graveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveyard :: LocationCard Graveyard
graveyard = location Graveyard Cards.graveyard 1 (PerPlayer 2)

instance HasAbilities Graveyard where
  getAbilities (Graveyard x) = withRevealedAbilities x [mkAbility x 1 $ forced $ Enters #after You (be x)]

instance RunMessage Graveyard where
  runMessage msg l@(Graveyard attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      rivertown <- getJustLocationByName "Rivertown"
      player <- getPlayer iid
      canMoveToRivertown <- getCanMoveTo iid attrs rivertown
      push
        $ chooseOne player
        $ [Label "Take 2 horror" [assignHorror iid (attrs.ability 1) 2]]
        <> [Label "Move to Rivertown" [MoveTo $ move (attrs.ability 1) iid rivertown] | canMoveToRivertown]
      pure l
    _ -> Graveyard <$> runMessage msg attrs
