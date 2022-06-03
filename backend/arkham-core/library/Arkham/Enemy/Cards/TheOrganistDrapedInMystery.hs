module Arkham.Enemy.Cards.TheOrganistDrapedInMystery
  ( theOrganistDrapedInMystery
  , TheOrganistDrapedInMystery(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Enemy.Helpers
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import qualified Arkham.Timing as Timing

newtype TheOrganistDrapedInMystery = TheOrganistDrapedInMystery EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor env TheOrganistDrapedInMystery where
  getModifiersFor _ target (TheOrganistDrapedInMystery attrs)
    | isTarget attrs target = pure $ toModifiers attrs [CannotBeDamaged]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TheOrganistDrapedInMystery where
  getAbilities (TheOrganistDrapedInMystery attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ PhaseEnds Timing.After $ PhaseIs
        EnemyPhase
    ]

theOrganistDrapedInMystery :: EnemyCard TheOrganistDrapedInMystery
theOrganistDrapedInMystery = enemy
  TheOrganistDrapedInMystery
  Cards.theOrganistDrapedInMystery
  (3, Static 1, 5)
  (0, 1)

instance EnemyRunner env => RunMessage TheOrganistDrapedInMystery where
  runMessage msg e@(TheOrganistDrapedInMystery attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      engagedInvestigators <-
        selectList $ InvestigatorEngagedWith $ EnemyWithId $ toId attrs
      if null engagedInvestigators
        then do
          leadInvestigatorId <- getLeadInvestigatorId
          mappings <- getList (EnemyWithId $ toId attrs)

          let
            minDistance = fromJustNote "error" . minimumMay $ map
              (unDistance . snd)
              mappings
            investigatorIds =
              map fst $ filter ((== minDistance) . unDistance . snd) mappings

          choices <- fmap (nub . concat) $ for investigatorIds $ \iid ->
            selectList $ LocationWithDistanceFrom
              (minDistance + 1)
              (LocationWithInvestigator $ InvestigatorWithId iid)

          emptyLocations <-
            selectList $ LocationWithoutInvestigators <> LocationMatchAny
              (map LocationWithId choices)
          let
            locations = if null emptyLocations then choices else emptyLocations

          push $ chooseOrRunOne
            leadInvestigatorId
            [ targetLabel
                location
                [MoveToward (toTarget attrs) (LocationWithId location)]
            | location <- locations
            ]
          pure e
        else
          e <$ pushAll
            [ DisengageEnemy iid $ toId attrs | iid <- engagedInvestigators ]
    _ -> TheOrganistDrapedInMystery <$> runMessage msg attrs
