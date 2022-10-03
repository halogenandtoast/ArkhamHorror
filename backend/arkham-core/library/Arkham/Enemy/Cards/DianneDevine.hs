module Arkham.Enemy.Cards.DianneDevine
  ( dianneDevine
  , DianneDevine(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype DianneDevine = DianneDevine EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianneDevine :: EnemyCard DianneDevine
dianneDevine = enemy DianneDevine Cards.dianneDevine (2, Static 3, 2) (0, 0)

instance HasModifiersFor DianneDevine where
  getModifiersFor (InvestigatorTarget iid) (DianneDevine a) = do
    affected <- iid <=~> InvestigatorAt (locationWithEnemy $ toId a)
    pure $ toModifiers a $ if affected
      then [CannotDiscoverClues, CannotTakeControlOfClues]
      else []
  getModifiersFor _ _ = pure []

instance HasAbilities DianneDevine where
  getAbilities (DianneDevine a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ PhaseBegins Timing.When $ PhaseIs
        EnemyPhase
    ]

instance RunMessage DianneDevine where
  runMessage msg e@(DianneDevine attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      locations <-
        selectList
          (LocationWithAsset $ AssetWithFewestClues $ AssetWithTrait Bystander)
      e <$ when
        (notNull locations)
        (push $ chooseOne
          leadInvestigatorId
          [ targetLabel location [EnemyMove (toId attrs) location]
          | location <- locations
          ]
        )
    _ -> DianneDevine <$> runMessage msg attrs
