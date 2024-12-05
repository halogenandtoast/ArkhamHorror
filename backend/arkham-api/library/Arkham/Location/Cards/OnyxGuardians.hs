module Arkham.Location.Cards.OnyxGuardians (onyxGuardians, OnyxGuardians (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (AncientOne))

newtype OnyxGuardians = OnyxGuardians LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onyxGuardians :: LocationCard OnyxGuardians
onyxGuardians =
  locationWith OnyxGuardians Cards.onyxGuardians 0 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ FullyFlooded)

instance HasAbilities OnyxGuardians where
  getAbilities (OnyxGuardians a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost
      $ PlaceKeyCost (toTarget a) YellowKey
      <> GroupClueCost (PerPlayer 1) (be a)

instance HasModifiersFor OnyxGuardians where
  getModifiersFor (OnyxGuardians a) = do
    phase <- getPhase
    if phase == #enemy
      then do
        ancientOnes <- select $ ReadyEnemy <> withTrait AncientOne
        modifySelect a (investigatorAt a) $ map AsIfEngagedWith ancientOnes
      else pure mempty

instance RunMessage OnyxGuardians where
  runMessage msg l@(OnyxGuardians attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (EnemyWithTrait AncientOne) $ automaticallyEvadeEnemy iid
      setThisFloodLevel attrs Unflooded
      gameModifier (attrs.ability 1) attrs CannotBeFlooded
      pure l
    _ -> OnyxGuardians <$> liftRunMessage msg attrs
