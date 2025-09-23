module Arkham.Treachery.Cards.SickeningWebs (sickeningWebs) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword (Keyword (Alert, Retaliate))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Spider))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SickeningWebs = SickeningWebs TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningWebs :: TreacheryCard SickeningWebs
sickeningWebs = treachery SickeningWebs Cards.sickeningWebs

instance HasModifiersFor SickeningWebs where
  getModifiersFor (SickeningWebs attrs) = do
    modifySelect
      attrs
      (EnemyWithTrait Spider <> at_ (locationWithTreachery attrs))
      [AddKeyword Retaliate, AddKeyword Alert]
    modifySelect attrs (InvestigatorAt (locationWithTreachery attrs)) [CannotMove]

instance HasAbilities SickeningWebs where
  getAbilities (SickeningWebs x) = [skillTestAbility $ restricted x 1 OnSameLocation actionAbility]

instance RunMessage SickeningWebs where
  runMessage msg t@(SickeningWebs attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- selectOne $ locationWithInvestigator iid <> LocationCanHaveAttachments
      for_ mlid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#combat, #agility] (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SickeningWebs <$> liftRunMessage msg attrs
