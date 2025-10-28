module Arkham.Location.Cards.TothisBarrens (tothisBarrens) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TothisBarrens = TothisBarrens LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tothisBarrens :: LocationCard TothisBarrens
tothisBarrens = location TothisBarrens Cards.tothisBarrens 3 (PerPlayer 1)

instance HasAbilities TothisBarrens where
  getAbilities (TothisBarrens a) =
    extendRevealed
      a
      [ restricted a 1 (thisExists a (LocationWithToken Shard) <> exists (enemyAt a <> ReadyEnemy))
          $ forced
          $ PhaseEnds #when #enemy
      , limitedAbility (MaxPer Cards.tothisBarrens PerGame 1)
          $ restricted
            a
            2
            (thisExists a (LocationWithToken Shard <> LocationWithoutClues))
            actionAbility
      ]

instance RunMessage TothisBarrens where
  runMessage msg l@(TothisBarrens attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeTokens (attrs.ability 1) attrs Shard 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      heliosTelescope <- selectJust $ assetIs Assets.heliosTelescopeGateToTheCosmos
      moveTokens (attrs.ability 2) attrs heliosTelescope Shard 1
      pure l
    _ -> TothisBarrens <$> liftRunMessage msg attrs
