module Arkham.Treachery.Cards.MemoryVariant (memoryVariant) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MemoryVariant = MemoryVariant TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryVariant :: TreacheryCard MemoryVariant
memoryVariant = treachery MemoryVariant Cards.memoryVariant

instance HasModifiersFor MemoryVariant where
  getModifiersFor (MemoryVariant a) = do
    modifySelect a AnyEvent [SetAfterPlay RemoveThisFromGame]

instance HasAbilities MemoryVariant where
  getAbilities (MemoryVariant a) =
    [ mkAbility a 1 $ forced $ PlayEvent #after You AnyEvent
    , skillTestAbility $ mkAbility a 2 actionAbility
    ]

instance RunMessage MemoryVariant where
  runMessage msg t@(MemoryVariant attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (getPlayedEvent -> eid) _ -> do
      hollow iid =<< fetchCard eid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> MemoryVariant <$> liftRunMessage msg attrs
