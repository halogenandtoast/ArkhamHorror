module Arkham.Types.Enemy.Cards.DianneDevine
  ( dianneDevine
  , DianneDevine(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

newtype DianneDevine = DianneDevine EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianneDevine :: EnemyCard DianneDevine
dianneDevine = enemy DianneDevine Cards.dianneDevine (2, Static 3, 2) (0, 0)

instance HasId LocationId env InvestigatorId => HasModifiersFor env DianneDevine where
  getModifiersFor _ (InvestigatorTarget iid) (DianneDevine a) = do
    lid <- getId iid
    pure $ toModifiers a $ if lid == enemyLocation a
      then [CannotDiscoverClues, CannotTakeControlOfClues]
      else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities DianneDevine where
  getAbilities (DianneDevine a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ PhaseBegins Timing.When $ PhaseIs
        EnemyPhase
    ]

instance EnemyRunner env => RunMessage env DianneDevine where
  runMessage msg e@(DianneDevine attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      locations <-
        selectList
          (LocationWithAsset $ AssetWithFewestClues $ AssetWithTrait Bystander)
      e <$ when
        (notNull locations)
        (push $ chooseOne
          leadInvestigatorId
          [ TargetLabel
              (LocationTarget location)
              [EnemyMove (toId attrs) (enemyLocation attrs) location]
          | location <- locations
          ]
        )
    _ -> DianneDevine <$> runMessage msg attrs
