module Arkham.Event.Cards.Interrogate (
  interrogate,
  Interrogate (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait (Trait (Humanoid))

newtype Interrogate = Interrogate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interrogate :: EventCard Interrogate
interrogate =
  event Interrogate Cards.interrogate

instance RunMessage Interrogate where
  runMessage msg e@(Interrogate attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      mlocation <- field InvestigatorLocation iid
      case mlocation of
        Just location -> do
          enemies <- select $ enemyAt location <> EnemyWithTrait Humanoid <> CanParleyEnemy iid
          player <- getPlayer iid
          pushAll
            [ chooseOne
                player
                [ targetLabel
                  enemy
                  [ parley
                      iid
                      (toSource attrs)
                      (EnemyTarget enemy)
                      SkillCombat
                      (SumCalculation [Fixed 3, EnemyFieldCalculation enemy EnemyHealthDamage])
                  ]
                | enemy <- enemies
                ]
            ]
        _ -> error "investigator not at location"
      pure e
    PassedSkillTest iid _ _ SkillTestInitiatorTarget {} _ _ -> do
      mlocation <- field InvestigatorLocation iid
      let
        matcher = case mlocation of
          Nothing -> LocationWithAnyClues
          Just lid -> LocationWithAnyClues <> NotLocation (LocationWithId lid)
      locations <- select matcher
      player <- getPlayer iid
      pushAll
        $ [InvestigatorDiscoverCluesAtTheirLocation iid (toSource attrs) 1 Nothing]
        <> [ chooseOrRunOne
              player
              [ targetLabel
                location
                [InvestigatorDiscoverClues iid location (toSource attrs) 1 Nothing]
              | location <- locations
              ]
           ]
      pure e
    _ -> Interrogate <$> runMessage msg attrs
