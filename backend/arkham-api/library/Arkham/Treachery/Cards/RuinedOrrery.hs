module Arkham.Treachery.Cards.RuinedOrrery (ruinedOrrery) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Message.Lifted.Log (record)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RuinedOrrery = RuinedOrrery TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinedOrrery :: TreacheryCard RuinedOrrery
ruinedOrrery = treachery RuinedOrrery Cards.ruinedOrrery

instance HasAbilities RuinedOrrery where
  getAbilities (RuinedOrrery a) = case a.attached.location of
    Just _ -> [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]
    Nothing -> []

instance RunMessage RuinedOrrery where
  runMessage msg t@(RuinedOrrery attrs) = runQueueT $ case msg of
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
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- Record "Y'ch'lecht" under rune_h, discovering this alien glyph, and add
      -- this card to the victory display.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_h" :: Text, "Y'ch'lecht" :: Text)
      addToVictory iid attrs
      pure t
    _ -> RuinedOrrery <$> liftRunMessage msg attrs
