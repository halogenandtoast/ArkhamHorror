module Arkham.Treachery.Cards.SeafloorFrieze (seafloorFrieze) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Story (readStory)
import Arkham.SkillTest.Type
import Arkham.SkillType
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SeafloorFrieze = SeafloorFrieze TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seafloorFrieze :: TreacheryCard SeafloorFrieze
seafloorFrieze = treachery SeafloorFrieze Cards.seafloorFrieze

instance HasModifiersFor SeafloorFrieze where
  getModifiersFor (SeafloorFrieze attrs) = modifySelf attrs [CannotLeavePlay]

instance HasAbilities SeafloorFrieze where
  getAbilities (SeafloorFrieze a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage SeafloorFrieze where
  runMessage msg t@(SeafloorFrieze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Revelation cannot be canceled; attach to your location. (CannotLeavePlay handled above.)
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure t
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) Initiator {} (SkillSkillTest SkillCombat) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure t
    PassedSkillTest
      iid
      _
      (isAbilitySource attrs 1 -> True)
      Initiator {}
      (SkillSkillTest SkillWillpower)
      _ -> do
        flipOver iid attrs
        pure t
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.seafloorFrieze
      pure t
    _ -> SeafloorFrieze <$> liftRunMessage msg attrs
