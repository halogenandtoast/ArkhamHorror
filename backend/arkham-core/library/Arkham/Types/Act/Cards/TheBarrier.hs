module Arkham.Types.Act.Cards.TheBarrier where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Timing qualified as Timing

newtype TheBarrier = TheBarrier ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
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

instance ActRunner env => RunMessage env TheBarrier where
  runMessage msg a@(TheBarrier attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid))
    AdvanceAct aid _ | aid == toId a && onSide B attrs -> do
      hallwayId <- getJustLocationIdByName "Hallway"
      parlorId <- getJustLocationIdByName "Parlor"
      ghoulPriest <- getSetAsideCard Enemies.ghoulPriest
      litaChantler <- getSetAsideCard Assets.litaChantler
      a <$ pushAll
        [ RevealLocation Nothing parlorId
        , CreateStoryAssetAt litaChantler parlorId
        , CreateEnemyAt ghoulPriest hallwayId Nothing
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
    _ -> TheBarrier <$> runMessage msg attrs
