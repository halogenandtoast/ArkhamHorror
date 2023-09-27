module Arkham.Event.Cards.EtherealForm (
  etherealForm,
  EtherealForm (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message

newtype EtherealForm = EtherealForm EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealForm :: EventCard EtherealForm
etherealForm =
  event EtherealForm Cards.etherealForm

instance RunMessage EtherealForm where
  runMessage msg e@(EtherealForm attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs iid (AddSkillValue #willpower)
        , chooseEvadeEnemy iid attrs #agility
        ]
      pure e
    _ -> EtherealForm <$> runMessage msg attrs
