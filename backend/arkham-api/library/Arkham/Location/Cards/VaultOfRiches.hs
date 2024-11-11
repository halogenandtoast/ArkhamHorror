module Arkham.Location.Cards.VaultOfRiches (vaultOfRiches, VaultOfRiches (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Phase

newtype VaultOfRiches = VaultOfRiches LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultOfRiches :: LocationCard VaultOfRiches
vaultOfRiches =
  locationWith VaultOfRiches Cards.vaultOfRiches 4 (PerPlayer 2)
    $ connectsToAdjacent
    . (floodLevelL ?~ PartiallyFlooded)

instance HasAbilities VaultOfRiches where
  getAbilities (VaultOfRiches a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> youExist (InvestigatorWithKey PurpleKey)) actionAbility

instance RunMessage VaultOfRiches where
  runMessage msg l@(VaultOfRiches attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeKey attrs PurpleKey
      enemies <- select NonEliteEnemy
      chooseTargetM iid enemies \enemy -> do
        automaticallyEvadeEnemy iid enemy
        push $ EnemyMove enemy attrs.id
        nextPhaseModifier UpkeepPhase (attrs.ability 1) enemy DoesNotReadyDuringUpkeep
      pure l
    _ -> VaultOfRiches <$> liftRunMessage msg attrs
