module Arkham.Act.Cards.TheChamberOfTheBeast (
  TheChamberOfTheBeast (..),
  theChamberOfTheBeast,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype TheChamberOfTheBeast = TheChamberOfTheBeast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChamberOfTheBeast :: ActCard TheChamberOfTheBeast
theChamberOfTheBeast =
  act (2, A) TheChamberOfTheBeast Cards.theChamberOfTheBeast Nothing

instance HasAbilities TheChamberOfTheBeast where
  getAbilities (TheChamberOfTheBeast x)
    | onSide A x =
        [ mkAbility x 1
            $ Objective
            $ ForcedAbility
            $ EnemyDefeated Timing.After Anyone ByAny
            $ enemyIs Cards.silasBishop
        , restrictedAbility
            x
            2
            ( LocationExists
                $ LocationWithTitle "The Hidden Chamber"
                <> LocationWithoutClues
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage TheChamberOfTheBeast where
  runMessage msg a@(TheChamberOfTheBeast attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      resolution <-
        maybe 3 (const 2)
          <$> selectOne (assetIs Cards.theNecronomiconOlausWormiusTranslation)
      push
        $ chooseOne
          lead
          [ Label
              ("Resolution " <> tshow resolution)
              [ScenarioResolution $ Resolution resolution]
          ]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push R1
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      push (AdvanceAct (toId attrs) source AdvancedWithOther)
      pure a
    _ -> TheChamberOfTheBeast <$> runMessage msg attrs
