module Arkham.Act.Cards.TheBarrier where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Placement
import Arkham.Source
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
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithClues)
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      hallwayId <- getJustLocationIdByName "Hallway"
      parlorId <- getJustLocationIdByName "Parlor"
      ghoulPriest <- getSetAsideCard Enemies.ghoulPriest
      litaChantler <- getSetAsideCard Assets.litaChantler
      a <$ pushAll
        [ RevealLocation Nothing parlorId
        , CreateAssetAt litaChantler (AtLocation parlorId)
        , CreateEnemyAt ghoulPriest hallwayId Nothing
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
    _ -> TheBarrier <$> runMessage msg attrs
