module Arkham.Location.Cards.NorthTower_287
  ( northTower_287
  , NorthTower_287(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Sequence ( AgendaSide (C) )
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype NorthTower_287 = NorthTower_287 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northTower_287 :: LocationCard NorthTower_287
northTower_287 = location NorthTower_287 Cards.northTower_287 2 (PerPlayer 2)

instance HasAbilities NorthTower_287 where
  getAbilities (NorthTower_287 a) = withBaseAbilities
    a
    [ restrictedAbility
        a
        1
        (InvestigatorExists $ InvestigatorAt $ LocationWithId $ toId a)
      $ ForcedAbility
      $ PlacedCounterOnAgenda
          Timing.After
          (AgendaWithSide C)
          DoomCounter
          (AtLeast $ Static 1)
    ]

instance RunMessage NorthTower_287 where
  runMessage msg l@(NorthTower_287 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      iids <- selectList $ InvestigatorAt $ LocationWithId $ toId attrs
      pushAll
        [ InvestigatorAssignDamage iid source DamageAny 1 0 | iid <- iids ]
      pure l
    _ -> NorthTower_287 <$> runMessage msg attrs
