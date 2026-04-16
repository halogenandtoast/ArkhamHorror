module Arkham.Location.Cards.FecundThicket (fecundThicket) where

import Arkham.Ability
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn

newtype FecundThicket = FecundThicket LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fecundThicket :: LocationCard FecundThicket
fecundThicket = locationWith FecundThicket Cards.fecundThicket 6 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor FecundThicket where
  getModifiersFor (FecundThicket a) = do
    darkness <- getDarknessLevel
    modifySelf a [ShroudModifier (-darkness)]

instance HasAbilities FecundThicket where
  getAbilities (FecundThicket a) =
    extendRevealed a [mkAbility a 1 $ forced $ RevealLocation #after You $ be a]

instance RunMessage FecundThicket where
  runMessage msg l@(FecundThicket attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      x <- getDarknessLevel
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed x)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      enemies <- pursuitEnemiesWithHighestEvade
      chooseTargetM iid enemies \e -> spawnAt e Nothing (SpawnAtLocation attrs.id)
      pure l
    _ -> FecundThicket <$> liftRunMessage msg attrs
