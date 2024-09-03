module Arkham.Location.Cards.Kitchen (kitchen, Kitchen (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = location Kitchen Cards.kitchen 2 (PerPlayer 1)

instance HasAbilities Kitchen where
  getAbilities (Kitchen attrs) =
    extendRevealed
      attrs
      [skillTestAbility $ restrictedAbility attrs 1 (Here <> NoCluesOnThis) actionAbility]

instance HasModifiersFor Kitchen where
  getModifiersFor (LocationTarget lid) (Kitchen attrs) | lid == toId attrs = do
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance RunMessage Kitchen where
  runMessage msg l@(Kitchen attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure l
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isAbilitySource attrs 1 source -> l <$ push (Remember SetAFireInTheKitchen)
    _ -> Kitchen <$> runMessage msg attrs
