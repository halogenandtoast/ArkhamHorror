module Arkham.Act.Cards.TheBarrier where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Placement

newtype TheBarrier = TheBarrier ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

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
  runMessage msg a@(TheBarrier attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ advanceVia #clues a iid
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      hallway <- getJustLocationByName "Hallway"
      parlor <- getJustLocationByName "Parlor"
      ghoulPriest <- getSetAsideCard Enemies.ghoulPriest
      litaChantler <- getSetAsideCard Assets.litaChantler
      createGhoulPriest <- createEnemyAt_ ghoulPriest hallway Nothing
      assetId <- getRandom

      pushAll
        [ RevealLocation Nothing parlor
        , CreateAssetAt assetId litaChantler (AtLocation parlor)
        , createGhoulPriest
        , advanceActDeck attrs
        ]
      pure a
    _ -> TheBarrier <$> runMessage msg attrs
