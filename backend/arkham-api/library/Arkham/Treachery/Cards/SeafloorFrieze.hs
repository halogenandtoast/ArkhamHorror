module Arkham.Treachery.Cards.SeafloorFrieze (seafloorFrieze) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Key
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Message.Lifted.Log (record)
import Arkham.SkillTest.Type
import Arkham.SkillType
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
      withLocationOf iid \lid -> attachTreachery attrs lid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Test [combat] (2), then test [willpower] (2). The willpower test is only
      -- begun if the combat test is passed, so a double-success is required.
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure t
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) Initiator {} (SkillSkillTest SkillCombat) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure t
    PassedSkillTest _iid _ (isAbilitySource attrs 1 -> True) Initiator {} (SkillSkillTest SkillWillpower) _ -> do
      -- Succeeded at both tests: flip this card and resolve its glyph translation.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_w" :: Text, "Parasite" :: Text)
      pure t
    _ -> SeafloorFrieze <$> liftRunMessage msg attrs
