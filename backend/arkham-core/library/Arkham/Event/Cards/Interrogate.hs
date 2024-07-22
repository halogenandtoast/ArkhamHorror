module Arkham.Event.Cards.Interrogate (
  interrogate,
  Interrogate (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
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
      location <- fieldJust InvestigatorLocation iid
      enemies <- select $ enemyAt location <> EnemyWithTrait Humanoid <> CanParleyEnemy iid
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ targetLabel
            enemy
            [ parley sid iid attrs enemy #combat
                $ SumCalculation [Fixed 3, EnemyFieldCalculation enemy EnemyHealthDamage]
            ]
          | enemy <- enemies
          ]
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
        $ [Msg.DiscoverClues iid $ discoverAtYourLocation (toSource attrs) 1]
        <> [ chooseOrRunOne
              player
              [ targetLabel
                location
                [Msg.DiscoverClues iid $ discover location (toSource attrs) 1]
              | location <- locations
              ]
           ]
      pure e
    _ -> Interrogate <$> runMessage msg attrs
