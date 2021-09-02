module Arkham.Types.Location.Cards.BridgeOfSighs
  ( bridgeOfSighs
  , BridgeOfSighs(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

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

instance HasAbilities env BridgeOfSighs where
  getAbilities iid window (BridgeOfSighs attrs) =
    withBaseAbilities iid window attrs $ pure
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
