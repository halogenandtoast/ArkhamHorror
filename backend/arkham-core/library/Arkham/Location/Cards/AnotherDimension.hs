module Arkham.Location.Cards.AnotherDimension
  ( anotherDimension
  , AnotherDimension(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( anotherDimension )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype AnotherDimension = AnotherDimension LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherDimension :: LocationCard AnotherDimension
anotherDimension =
  location AnotherDimension Cards.anotherDimension 6 (Static 0)

instance HasAbilities AnotherDimension where
  getAbilities (AnotherDimension attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ LocationLeavesPlay Timing.When
          $ LocationMatchAny
              [LocationWithEnemy AnyEnemy, LocationWithInvestigator Anyone]
        | locationRevealed attrs
        ]

instance RunMessage AnotherDimension where
  runMessage msg l@(AnotherDimension attrs) = case msg of
    UseCardAbility _ source [Window _ (LeavePlay (LocationTarget lid))] 1 _
      | isSource attrs source -> do
        investigatorIds <- selectList $ InvestigatorAt $ LocationWithId lid
        enemyIds <- selectList $ UnengagedEnemy <> EnemyAt (LocationWithId lid)
        l <$ pushAll
          ([ MoveTo (toSource attrs) iid (toId attrs) | iid <- investigatorIds ]
          <> [ EnemyMove eid (toId attrs) | eid <- enemyIds ]
          )
    _ -> AnotherDimension <$> runMessage msg attrs
