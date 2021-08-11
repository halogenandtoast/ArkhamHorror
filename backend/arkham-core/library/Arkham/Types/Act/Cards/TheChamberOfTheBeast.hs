module Arkham.Types.Act.Cards.TheChamberOfTheBeast
  ( TheChamberOfTheBeast(..)
  , theChamberOfTheBeast
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Resolution
import Arkham.Types.Restriction
import qualified Arkham.Types.Timing as Timing

newtype TheChamberOfTheBeast = TheChamberOfTheBeast ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

theChamberOfTheBeast :: ActCard TheChamberOfTheBeast
theChamberOfTheBeast =
  act (2, A) TheChamberOfTheBeast Cards.theChamberOfTheBeast Nothing

instance HasActions TheChamberOfTheBeast where
  getActions (TheChamberOfTheBeast x) =
    [ mkAbility
        x
        1
        (Objective $ ForcedAbility $ EnemyDefeated
          Timing.When
          Anyone
          (enemyIs Cards.silasBishop)
        )
      , restrictedAbility
        x
        2
        (LocationExists
          (LocationWithTitle "The Hidden Chamber" <> LocationWithoutClues)
        )
        (Objective $ ForcedAbility AnyWindow)
      ]
      <> getActions x

instance ActRunner env => RunMessage env TheChamberOfTheBeast where
  runMessage msg a@(TheChamberOfTheBeast attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (ScenarioResolution $ Resolution 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (AdvanceAct actId source)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
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
    _ -> TheChamberOfTheBeast <$> runMessage msg attrs
