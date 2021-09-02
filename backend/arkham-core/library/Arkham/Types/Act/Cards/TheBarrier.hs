module Arkham.Types.Act.Cards.TheBarrier where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing

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
      ghoulPriest <- EncounterCard <$> genEncounterCard Enemies.ghoulPriest
      litaChantler <- PlayerCard <$> genPlayerCard Assets.litaChantler
      parlorId <- getJustLocationIdByName "Parlor"
      a <$ pushAll
        [ RevealLocation Nothing parlorId
        , CreateStoryAssetAt litaChantler parlorId
        , CreateEnemyAt ghoulPriest hallwayId Nothing
        , NextAct aid "01110"
        ]
    _ -> TheBarrier <$> runMessage msg attrs
