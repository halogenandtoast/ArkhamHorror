module Arkham.Location.Cards.TheGuardian (
  theGuardian,
  TheGuardian (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype TheGuardian = TheGuardian LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theGuardian :: LocationCard TheGuardian
theGuardian =
  locationWith
    TheGuardian
    Cards.theGuardian
    3
    (PerPlayer 2)
    (connectsToL .~ singleton RightOf)

instance HasAbilities TheGuardian where
  getAbilities (TheGuardian attrs) =
    withRevealedAbilities attrs
      $ [mkAbility attrs 1 $ ReactionAbility (Enters Timing.After You $ LocationWithId $ toId attrs) Free]

instance RunMessage TheGuardian where
  runMessage msg l@(TheGuardian attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure l
    _ -> TheGuardian <$> runMessage msg attrs
