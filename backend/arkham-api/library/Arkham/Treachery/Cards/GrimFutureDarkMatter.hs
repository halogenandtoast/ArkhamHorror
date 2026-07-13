module Arkham.Treachery.Cards.GrimFutureDarkMatter (grimFutureDarkMatter) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GrimFutureDarkMatter = GrimFutureDarkMatter TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grimFutureDarkMatter :: TreacheryCard GrimFutureDarkMatter
grimFutureDarkMatter = treachery GrimFutureDarkMatter Cards.grimFutureDarkMatter

instance HasAbilities GrimFutureDarkMatter where
  getAbilities (GrimFutureDarkMatter a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ OrWindowMatcher [AgendaAdvances #when AnyAgenda, ActAdvances #when AnyAct]
    ]

instance RunMessage GrimFutureDarkMatter where
  runMessage msg t@(GrimFutureDarkMatter attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- getInvestigators
      chooseOrRunOneM iid $ targets investigators (placeInThreatArea attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> GrimFutureDarkMatter <$> liftRunMessage msg attrs
