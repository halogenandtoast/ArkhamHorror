module Arkham.Location.Cards.TheGuardian (theGuardian, TheGuardian (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TheGuardian = TheGuardian LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGuardian :: LocationCard TheGuardian
theGuardian =
  locationWith TheGuardian Cards.theGuardian 3 (PerPlayer 2) (connectsToL .~ singleton RightOf)

instance HasAbilities TheGuardian where
  getAbilities (TheGuardian attrs) =
    extendRevealed attrs [mkAbility attrs 1 $ freeReaction (Enters #after You $ be attrs)]

instance RunMessage TheGuardian where
  runMessage msg l@(TheGuardian attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (attrs.ability 1) 1
      pure l
    _ -> TheGuardian <$> runMessage msg attrs
