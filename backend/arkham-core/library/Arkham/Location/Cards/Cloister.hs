module Arkham.Location.Cards.Cloister (cloister, Cloister (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype Cloister = Cloister LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloister :: LocationCard Cloister
cloister = location Cloister Cards.cloister 2 (PerPlayer 1)

instance HasAbilities Cloister where
  getAbilities (Cloister a) =
    withBaseAbilities a [restrictedAbility a 1 (Here <> NoCluesOnThis) parleyAction_]

instance RunMessage Cloister where
  runMessage msg l@(Cloister attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      sid <- getRandom
      push $ parley sid iid source iid #willpower (Fixed 3)
      pure l
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> l <$ push (Remember FoundAGuide)
    _ -> Cloister <$> runMessage msg attrs
