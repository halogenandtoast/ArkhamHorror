module Arkham.Treachery.Cards.ChildrenOfBlood.BloodBlight.BlightedBlood (blightedBlood) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodBlight qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BlightedBlood = BlightedBlood TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blightedBlood :: TreacheryCard BlightedBlood
blightedBlood = treachery BlightedBlood Cards.blightedBlood

instance HasModifiersFor BlightedBlood where
  getModifiersFor (BlightedBlood a) =
    inThreatAreaGets a [SkillModifier #intellect (-1), SanityModifier (-1)]

instance HasAbilities BlightedBlood where
  getAbilities (BlightedBlood a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage BlightedBlood where
  runMessage msg t@(BlightedBlood attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sealed <- selectAny $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      if sealed
        then placeInThreatArea attrs iid
        else selectOne (OnlyInBag #blood) >>= traverse_ (sealChaosToken iid iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> BlightedBlood <$> liftRunMessage msg attrs
