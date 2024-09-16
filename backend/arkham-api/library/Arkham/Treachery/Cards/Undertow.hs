module Arkham.Treachery.Cards.Undertow (undertow, Undertow (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Undertow = Undertow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undertow :: TreacheryCard Undertow
undertow = treachery Undertow Cards.undertow

instance HasAbilities Undertow where
  getAbilities (Undertow a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ forced $ Moves #after You AnySource Anywhere Anywhere
    , skillTestAbility $ restrictedAbility a 2 OnSameLocation $ FastAbility $ HandDiscardCost 1 #any
    ]

instance RunMessage Undertow where
  runMessage msg t@(Undertow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectAny (locationWithInvestigator iid <> FloodedLocation) >>= \case
        True -> placeInThreatArea attrs iid
        False -> gainSurge attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 2 2
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] $ \skill ->
          skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Undertow <$> liftRunMessage msg attrs
