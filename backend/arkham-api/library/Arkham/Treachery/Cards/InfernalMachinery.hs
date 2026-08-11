module Arkham.Treachery.Cards.InfernalMachinery (infernalMachinery) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Artifact, Glyph))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InfernalMachinery = InfernalMachinery TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infernalMachinery :: TreacheryCard InfernalMachinery
infernalMachinery = treachery InfernalMachinery Cards.infernalMachinery

instance HasModifiersFor InfernalMachinery where
  getModifiersFor (InfernalMachinery a) = case a.placement of
    InThreatArea iid ->
      modified_
        a
        iid
        [CannotTriggerAbilityMatching $ AbilityOnCard $ mapOneOf CardWithTrait [Glyph, Artifact]]
    _ -> pure ()

instance HasAbilities InfernalMachinery where
  getAbilities (InfernalMachinery a) =
    [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage InfernalMachinery where
  runMessage msg t@(InfernalMachinery attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#intellect, #agility] (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> InfernalMachinery <$> liftRunMessage msg attrs
