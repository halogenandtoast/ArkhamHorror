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
import Arkham.Message
import Arkham.Placement
import Arkham.Timing qualified as Timing

newtype TheBarrier = TheBarrier ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBarrier :: ActCard TheBarrier
theBarrier = act (2, A) TheBarrier Cards.theBarrier Nothing

instance HasAbilities TheBarrier where
  getAbilities (TheBarrier x) =
    [ mkAbility x 1
        $ Objective
        $ ReactionAbility (RoundEnds Timing.When)
        $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Hallway")
    ]

instance RunMessage TheBarrier where
  runMessage msg a@(TheBarrier attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId a) (toSource iid) AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      hallwayId <- getJustLocationByName "Hallway"
      parlorId <- getJustLocationByName "Parlor"
      ghoulPriest <- getSetAsideCard Enemies.ghoulPriest
      litaChantler <- getSetAsideCard Assets.litaChantler
      createGhoulPriest <- createEnemyAt_ ghoulPriest hallwayId Nothing
      assetId <- getRandom

      pushAll
        [ RevealLocation Nothing parlorId
        , CreateAssetAt assetId litaChantler (AtLocation parlorId)
        , createGhoulPriest
        , advanceActDeck attrs
        ]
      pure a
    _ -> TheBarrier <$> runMessage msg attrs
