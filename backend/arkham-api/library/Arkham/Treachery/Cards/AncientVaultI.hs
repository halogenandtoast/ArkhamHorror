module Arkham.Treachery.Cards.AncientVaultI (ancientVaultI) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultI = AncientVaultI TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultI :: TreacheryCard AncientVaultI
ancientVaultI = treachery AncientVaultI Cards.ancientVaultI

instance HasAbilities AncientVaultI where
  getAbilities (AncientVaultI a) = case a.attached.location of
    Just lid -> [skillTestAbility $ restricted a 1 (OnLocation $ LocationWithId lid) actionAbility]
    Nothing -> []

instance RunMessage AncientVaultI where
  runMessage msg t@(AncientVaultI attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- If drawn from the encounter discard pile, attach it to your location;
      -- otherwise discard it and it gains surge.
      if attrs.drawnFrom == Just Deck.EncounterDiscard
        then withLocationOf iid (attachTreachery attrs)
        else do
          gainSurge attrs
          toDiscard attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- Record "Cthulhu" under rune_i, discovering this alien glyph, and add
      -- this card to the victory display.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_i" :: Text, "Cthulhu" :: Text)
      addToVictory iid attrs
      pure t
    _ -> AncientVaultI <$> liftRunMessage msg attrs
