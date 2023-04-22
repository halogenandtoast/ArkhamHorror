module Arkham.Location.Cards.OuterWall_285
  ( outerWall_285
  , OuterWall_285(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Sequence ( AgendaSide (A) )
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype OuterWall_285 = OuterWall_285 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerWall_285 :: LocationCard OuterWall_285
outerWall_285 = location OuterWall_285 Cards.outerWall_285 2 (PerPlayer 2)

instance HasAbilities OuterWall_285 where
  getAbilities (OuterWall_285 a) = withBaseAbilities
    a
    [ restrictedAbility
        a
        1
        (InvestigatorExists $ InvestigatorAt $ LocationWithId $ toId a)
      $ ForcedAbility
      $ PlacedCounterOnAgenda
          Timing.After
          (AgendaWithSide A)
          DoomCounter
          (AtLeast $ Static 1)
    ]

instance RunMessage OuterWall_285 where
  runMessage msg l@(OuterWall_285 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      iids <- selectList $ InvestigatorAt $ LocationWithId $ toId attrs
      pushAll
        [ InvestigatorAssignDamage iid source DamageAny 1 0 | iid <- iids ]
      pure l
    _ -> OuterWall_285 <$> runMessage msg attrs
