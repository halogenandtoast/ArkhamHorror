module Arkham.Types.Location.Cards.BridgeOfSighs
  ( bridgeOfSighs
  , BridgeOfSighs(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype BridgeOfSighs = BridgeOfSighs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bridgeOfSighs :: LocationCard BridgeOfSighs
bridgeOfSighs = locationWith
  BridgeOfSighs
  Cards.bridgeOfSighs
  1
  (Static 2)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities BridgeOfSighs where
  getAbilities (BridgeOfSighs attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1
        $ ForcedAbility
        $ Leaves Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env BridgeOfSighs where
  runMessage msg l@(BridgeOfSighs attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> BridgeOfSighs <$> runMessage msg attrs
