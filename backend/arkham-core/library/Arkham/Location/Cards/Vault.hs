module Arkham.Location.Cards.Vault (
  vault,
  Vault (..),
)
where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Timing qualified as Timing

newtype Vault = Vault LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

vault :: LocationCard Vault
vault = location Vault Cards.vault 4 (PerPlayer 1)

instance HasModifiersFor Vault where
  getModifiersFor (InvestigatorTarget iid) (Vault attrs) = do
    hasElderThingKey <- iid <=~> InvestigatorWithKey ElderThingKey
    pure $ toModifiers attrs [CannotEnter (toId attrs) | unrevealed attrs && not hasElderThingKey]
  getModifiersFor _ _ = pure []

instance HasAbilities Vault where
  getAbilities (Vault attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ RevealLocation Timing.After You $ LocationWithId $ toId attrs]

instance RunMessage Vault where
  runMessage msg l@(Vault attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mKey <- getRandomKey
      for_ mKey $ \key ->
        push $ PlaceKey (toTarget attrs) key
      pure l
    _ -> Vault <$> runMessage msg attrs
