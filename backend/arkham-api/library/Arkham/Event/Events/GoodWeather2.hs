module Arkham.Event.Events.GoodWeather2 (goodWeather2) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher.Investigator
import Arkham.Matcher.Window

newtype GoodWeather2 = GoodWeather2 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goodWeather2 :: EventCard GoodWeather2
goodWeather2 = event GoodWeather2 Cards.goodWeather2

instance HasModifiersFor GoodWeather2 where
  getModifiersFor (GoodWeather2 a) = for_ (getEventMeta a) \sKind ->
    modifySelect a UneliminatedInvestigator [SkillModifier sKind 2]

instance HasAbilities GoodWeather2 where
  getAbilities (GoodWeather2 x) = [restricted x 1 OwnsThis $ forced $ PhaseBegins #when #investigation]

instance RunMessage GoodWeather2 where
  runMessage msg e@(GoodWeather2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      place attrs NextToAgenda
      chooseOneM iid do
        for_ [minBound ..] \sKind ->
          skillLabeled sKind $ push $ ForSkillType sKind msg
      pure e
    ForSkillType sKind (PlayThisEvent _iid (is attrs -> True)) -> do
      pure $ GoodWeather2 $ attrs & setMeta sKind
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> GoodWeather2 <$> liftRunMessage msg attrs
