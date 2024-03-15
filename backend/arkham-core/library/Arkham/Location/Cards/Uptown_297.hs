module Arkham.Location.Cards.Uptown_297 (
  uptown_297,
  Uptown_297 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype Uptown_297 = Uptown_297 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptown_297 :: LocationCard Uptown_297
uptown_297 = location Uptown_297 Cards.uptown_297 4 (Static 0)

instance HasAbilities Uptown_297 where
  getAbilities (Uptown_297 attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ ActionAbility [] $ ActionCost 1]

instance RunMessage Uptown_297 where
  runMessage msg l@(Uptown_297 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (attrs.ability 1) attrs #agility 2
      pure l
    PassedSkillTest _iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ n -> do
      let n' = min (maybe 0 countBreaches $ locationBreaches attrs) n
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) n', PlaceBreaches (toTarget act) n']
      pure l
    _ -> Uptown_297 <$> runMessage msg attrs
