module Arkham.Act.Cards.TheBarrier (theBarrier) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Placement

newtype TheBarrier = TheBarrier ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBarrier :: ActCard TheBarrier
theBarrier = act (2, A) TheBarrier Cards.theBarrier Nothing

instance HasAbilities TheBarrier where
  getAbilities (TheBarrier x) =
    [ mkAbility x 1
        $ Objective
        $ ReactionAbility (RoundEnds #when)
        $ GroupClueCost (PerPlayer 3) "Hallway"
    ]

instance RunMessage TheBarrier where
  runMessage msg a@(TheBarrier attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      parlor <- getJustLocationByName "Parlor"
      reveal parlor
      createAssetAt_ Assets.litaChantler (AtLocation parlor)
      createEnemyAt_ Enemies.ghoulPriest =<< getJustLocationByName "Hallway"
      advanceActDeck attrs
      pure a
    _ -> TheBarrier <$> liftRunMessage msg attrs
