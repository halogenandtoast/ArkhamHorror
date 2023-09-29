module Arkham.Event.Cards.MoneyTalks (
  moneyTalks,
  MoneyTalks (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type

newtype MoneyTalks = MoneyTalks EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moneyTalks :: EventCard MoneyTalks
moneyTalks =
  event MoneyTalks Cards.moneyTalks

instance RunMessage MoneyTalks where
  runMessage msg e@(MoneyTalks attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ ChangeSkillTestType ResourceSkillTest (HalfResourcesOf iid)
      pure e
    _ -> MoneyTalks <$> runMessage msg attrs
