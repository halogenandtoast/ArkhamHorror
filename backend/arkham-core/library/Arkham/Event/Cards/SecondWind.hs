module Arkham.Event.Cards.SecondWind
  ( secondWind
  , SecondWind(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History
import Arkham.Message
import Arkham.Target

newtype SecondWind = SecondWind EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondWind :: EventCard SecondWind
secondWind = event SecondWind Cards.secondWind

instance RunMessage SecondWind where
  runMessage msg e@(SecondWind attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      roundHistory <- getHistory RoundHistory iid
      let
        damageToHeal =
          if null (historyTreacheriesDrawn roundHistory) then 1 else 2
      e <$ pushAll
        [ HealDamage (InvestigatorTarget iid) damageToHeal
        , drawCards iid attrs 1
        , Discard (toTarget attrs)
        ]
    _ -> SecondWind <$> runMessage msg attrs
