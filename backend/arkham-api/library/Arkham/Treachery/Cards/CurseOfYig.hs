module Arkham.Treachery.Cards.CurseOfYig (curseOfYig, CurseOfYig (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Placement
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CurseOfYig = CurseOfYig TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfYig :: TreacheryCard CurseOfYig
curseOfYig = treachery CurseOfYig Cards.curseOfYig

instance HasModifiersFor CurseOfYig where
  getModifiersFor (CurseOfYig a) = case a.placement of
    InThreatArea iid -> modified_ a iid [SkillModifier #combat (-1), HealthModifier (-1), AddTrait Serpent]
    _ -> pure mempty

instance HasAbilities CurseOfYig where
  getAbilities (CurseOfYig a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage CurseOfYig where
  runMessage msg t@(CurseOfYig attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      checkDefeated attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- getVengeanceInVictoryDisplay
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed $ 2 + n)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CurseOfYig <$> liftRunMessage msg attrs
