module Arkham.Treachery.Cards.AncientVaultN (ancientVaultN) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultN = AncientVaultN TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultN :: TreacheryCard AncientVaultN
ancientVaultN = treachery AncientVaultN Cards.ancientVaultN

instance HasAbilities AncientVaultN where
  getAbilities (AncientVaultN a) = [restricted a 1 OnSameLocation actionAbility]

instance RunMessage AncientVaultN where
  runMessage msg t@(AncientVaultN attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Attach to your location. Cannot be canceled (revelations are not
      -- cancelable here, so no extra handling is required).
      withLocationOf iid (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Cost: take a combined total of X damage and/or horror, where X is the
      -- attached location's shroud. The player chooses how to split the total.
      x <- case attrs.placement of
        AttachedToLocation lid -> fieldWithDefault 0 LocationShroud lid
        _ -> pure 0
      chooseAmounts
        iid
        "Take a combined total of damage and/or horror"
        (TotalAmountTarget x)
        [("Damage", (0, x)), ("Horror", (0, x))]
        (toTarget attrs)
      pure t
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let damage = getChoiceAmount "Damage" choices
      let horror = getChoiceAmount "Horror" choices
      assignDamageAndHorror iid (attrs.ability 1) damage horror
      -- Flip this card over and resolve its text (the glyph translation).
      -- TODO: verify the exact glyph identifier string; the recorded word is "Doom".
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_n" :: Text, "Doom" :: Text)
      addToVictory iid attrs
      pure t
    _ -> AncientVaultN <$> liftRunMessage msg attrs
