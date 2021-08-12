module Arkham.Types.Event.Cards.SecondWind
  ( secondWind
  , SecondWind(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.History
import Arkham.Types.Message
import Arkham.Types.Target

newtype SecondWind = SecondWind EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondWind :: EventCard SecondWind
secondWind = event SecondWind Cards.secondWind

instance HasActions SecondWind
instance HasModifiersFor env SecondWind

instance (HasQueue env, HasHistory env) => RunMessage env SecondWind where
  runMessage msg e@(SecondWind attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      roundHistory <- getHistory RoundHistory iid
      let
        damageToHeal =
          if null (historyTreacheriesDrawn roundHistory) then 1 else 2
      e <$ pushAll
        [ HealDamage (InvestigatorTarget iid) damageToHeal
        , DrawCards iid 1 False
        , Discard (toTarget attrs)
        ]
    _ -> SecondWind <$> runMessage msg attrs
