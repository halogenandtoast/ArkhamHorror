module Arkham.Asset.Assets.MiasmicCrystalStrangeEvidence (miasmicCrystalStrangeEvidence) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Helpers.Window (cardDrawnBy)
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype MiasmicCrystalStrangeEvidence = MiasmicCrystalStrangeEvidence AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miasmicCrystalStrangeEvidence :: AssetCard MiasmicCrystalStrangeEvidence
miasmicCrystalStrangeEvidence = asset MiasmicCrystalStrangeEvidence Cards.miasmicCrystalStrangeEvidence

-- NOTE: this used to have noLimit, but I do not recall why
instance HasAbilities MiasmicCrystalStrangeEvidence where
  getAbilities (MiasmicCrystalStrangeEvidence a) =
    [ storyControlled_ a 1
        $ triggered
          (DrawCard #when (affectsOthers $ at_ YourLocation) (basic $ CardFromEncounterSet Tekelili) AnyDeck)
          (assetUseCost a Charge 1)
    ]

instance RunMessage MiasmicCrystalStrangeEvidence where
  runMessage msg a@(MiasmicCrystalStrangeEvidence attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawnBy -> (iid', card)) _ -> do
      cancelCardEffects (attrs.ability 1) card
      quietCancelCardDraw card
      putCardOnBottomOfDeck iid' TekeliliDeck card
      drawCards iid' (attrs.ability 1) 1
      pure a
    _ -> MiasmicCrystalStrangeEvidence <$> liftRunMessage msg attrs
