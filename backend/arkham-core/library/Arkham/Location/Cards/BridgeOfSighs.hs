module Arkham.Location.Cards.BridgeOfSighs
  ( bridgeOfSighs
  , BridgeOfSighs(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype BridgeOfSighs = BridgeOfSighs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bridgeOfSighs :: LocationCard BridgeOfSighs
bridgeOfSighs = locationWith
  BridgeOfSighs
  Cards.bridgeOfSighs
  1
  (Static 2)
  (connectsToL .~ singleton RightOf)

instance HasAbilities BridgeOfSighs where
  getAbilities (BridgeOfSighs attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ Leaves Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage BridgeOfSighs where
  runMessage msg l@(BridgeOfSighs attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> BridgeOfSighs <$> runMessage msg attrs
