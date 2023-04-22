module Arkham.Location.Cards.Atlantis
  ( atlantis
  , Atlantis(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype Atlantis = Atlantis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atlantis :: LocationCard Atlantis
atlantis = location Atlantis Cards.atlantis 3 (Static 2)

instance HasAbilities Atlantis where
  getAbilities (Atlantis a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ RevealChaosToken Timing.After Anyone
      $ TokenFaceIs AutoFail
    ]

instance RunMessage Atlantis where
  runMessage msg l@(Atlantis attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      pushAll [PlaceDoom (toTarget attrs) 1, stepMessage 1 msg]
      pure l
    UseCardAbilityStep _ _ 1 _ _ 1 -> do
      n <- field LocationDoom (toId attrs)
      when (n >= 3) $ push $ RemoveLocation (toId attrs)
      pure l
    _ -> Atlantis <$> runMessage msg attrs
