module Arkham.Types.Act.Cards.TheChamberOfTheBeast
  ( TheChamberOfTheBeast(..)
  , theChamberOfTheBeast
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Resolution
import Arkham.Types.Timing qualified as Timing

newtype TheChamberOfTheBeast = TheChamberOfTheBeast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChamberOfTheBeast :: ActCard TheChamberOfTheBeast
theChamberOfTheBeast =
  act (2, A) TheChamberOfTheBeast Cards.theChamberOfTheBeast Nothing

instance HasAbilities TheChamberOfTheBeast where
  getAbilities (TheChamberOfTheBeast x) =
    [ mkAbility x 1
    $ Objective
    $ ForcedAbility
    $ EnemyDefeated Timing.After Anyone
    $ enemyIs Cards.silasBishop
    , restrictedAbility
      x
      2
      (LocationExists
      $ LocationWithTitle "The Hidden Chamber"
      <> LocationWithoutClues
      )
    $ Objective
    $ ForcedAbility AnyWindow
    ]

instance ActRunner env => RunMessage env TheChamberOfTheBeast where
  runMessage msg a@(TheChamberOfTheBeast attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      resolution <- maybe 3 (const 2)
        <$> selectOne (assetIs Cards.theNecronomiconOlausWormiusTranslation)
      a <$ push
        (chooseOne
          leadInvestigatorId
          [ Label
              ("Resolution " <> tshow resolution)
              [ScenarioResolution $ Resolution resolution]
          ]
        )
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (ScenarioResolution $ Resolution 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    _ -> TheChamberOfTheBeast <$> runMessage msg attrs
