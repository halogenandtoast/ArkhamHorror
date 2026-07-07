module Arkham.Treachery.Cards.AncientVaultO (ancientVaultO) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultO = AncientVaultO TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultO :: TreacheryCard AncientVaultO
ancientVaultO = treachery AncientVaultO Cards.ancientVaultO

-- The card flips to its Glyph back side once translated; track that in meta so
-- the front-side action is no longer offered afterward.
flipped :: TreacheryAttrs -> Bool
flipped a = toResultDefault False a.meta

instance HasAbilities AncientVaultO where
  getAbilities (AncientVaultO a) = case a.attached.location of
    Just lid | not (flipped a) ->
      [ restricted
          a
          1
          ( OnLocation (LocationWithId lid)
              <> youExist (ControlsAsset DiscardableAsset)
          )
          actionAbility
      ]
    _ -> []

instance RunMessage AncientVaultO where
  runMessage msg t@(AncientVaultO attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Revelation cannot be canceled (handled via the card def's
      -- CannotBeCanceledRevelation); attach to your location.
      withLocationOf iid \lid -> attachTreachery attrs lid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- X = the attached location's shroud. Choose and discard assets you
      -- control with total printed resource cost >= X, then flip this card.
      x <- case attrs.attached.location of
        Just lid -> fieldWithDefault 0 LocationShroud lid
        Nothing -> pure 0
      total <-
        sum
          . map (printedCardCost . snd)
          <$> selectWithField AssetCard (assetControlledBy iid <> DiscardableAsset)
      when (total >= x) $ doStep x msg
      pure t
    DoStep x inner@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      if x <= 0
        then flipOver iid attrs
        else do
          assets <- selectWithField AssetCard (assetControlledBy iid <> DiscardableAsset)
          unless (null assets) do
            chooseOneM iid $ for_ assets \(aid, card) ->
              targeting aid do
                toDiscard (attrs.ability 1) aid
                doStep (x - printedCardCost card) inner
      pure t
    Flip _iid _ (isTarget attrs -> True) -> do
      -- "Flip this card over and resolve its text." The back side (11608b) is a
      -- Glyph; the known resolvable effect is translating its alien glyph.
      -- TODO: confirm the exact glyph letter/translated word printed on 11608b
      -- (rune_o). Placeholder "Door" used until verified.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_o" :: Text, "Power" :: Text)
      pure $ AncientVaultO $ attrs & setMeta True
    _ -> AncientVaultO <$> liftRunMessage msg attrs
