module Arkham.Event.Cards.MoneyTalks2 (
  moneyTalks2,
  MoneyTalks2 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type

newtype MoneyTalks2 = MoneyTalks2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moneyTalks2 :: EventCard MoneyTalks2
moneyTalks2 =
  event MoneyTalks2 Cards.moneyTalks2

instance RunMessage MoneyTalks2 where
  runMessage msg e@(MoneyTalks2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      pushAll [ChangeSkillTestType ResourceSkillTest (HalfResourcesOf iid), drawing]
      pure e
    _ -> MoneyTalks2 <$> runMessage msg attrs
