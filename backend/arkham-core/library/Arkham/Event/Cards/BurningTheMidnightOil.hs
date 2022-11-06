module Arkham.Event.Cards.BurningTheMidnightOil
  ( burningTheMidnightOil
  , BurningTheMidnightOil(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Message
import Arkham.SkillType

newtype BurningTheMidnightOil = BurningTheMidnightOil EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burningTheMidnightOil :: EventCard BurningTheMidnightOil
burningTheMidnightOil = event BurningTheMidnightOil Cards.burningTheMidnightOil

instance RunMessage BurningTheMidnightOil where
  runMessage msg e@(BurningTheMidnightOil attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      pushAll
        [ TakeResources iid 2 False
        , Investigate iid lid (toSource attrs) Nothing SkillIntellect False
        , Discard (toTarget attrs)
        ]
      pure e
    _ -> BurningTheMidnightOil <$> runMessage msg attrs
