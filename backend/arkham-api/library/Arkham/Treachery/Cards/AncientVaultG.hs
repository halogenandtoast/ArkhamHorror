module Arkham.Treachery.Cards.AncientVaultG (ancientVaultG) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Message.Lifted.Log (record)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultG = AncientVaultG TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultG :: TreacheryCard AncientVaultG
ancientVaultG = treachery AncientVaultG Cards.ancientVaultG

instance HasAbilities AncientVaultG where
  getAbilities (AncientVaultG a) = case a.attached.location of
    Just _ -> [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]
    Nothing -> []

instance RunMessage AncientVaultG where
  runMessage msg t@(AncientVaultG attrs) = runQueueT $ case msg of
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
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- Record "Life" under rune_g, discovering this alien glyph, and add this
      -- card to the victory display.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_g" :: Text, "Life" :: Text)
      addToVictory iid attrs
      pure t
    _ -> AncientVaultG <$> liftRunMessage msg attrs
