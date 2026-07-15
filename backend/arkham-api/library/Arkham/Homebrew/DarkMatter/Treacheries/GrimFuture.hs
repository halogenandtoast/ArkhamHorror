module Arkham.Homebrew.DarkMatter.Treacheries.GrimFuture (grimFuture) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GrimFuture = GrimFuture TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grimFuture :: TreacheryCard GrimFuture
grimFuture = treachery GrimFuture Cards.grimFuture

instance HasAbilities GrimFuture where
  getAbilities (GrimFuture a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ OrWindowMatcher [AgendaAdvances #when AnyAgenda, ActAdvances #when AnyAct]
    ]

instance RunMessage GrimFuture where
  runMessage msg t@(GrimFuture attrs) = runQueueT $ case msg of
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
    _ -> GrimFuture <$> liftRunMessage msg attrs
