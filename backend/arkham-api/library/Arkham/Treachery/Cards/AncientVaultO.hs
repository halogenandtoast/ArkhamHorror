module Arkham.Treachery.Cards.AncientVaultO (ancientVaultO) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Story.Cards qualified as Stories
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
    Just lid
      | not (flipped a) ->
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
      withLocationOf iid $ attachTreachery attrs
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
    Flip iid _ (isTarget attrs -> True) -> do
      -- "Flip this card over and resolve its text." The back (11608b) is a story
      -- card that translates the glyph and adds itself to the victory display. A
      -- treachery has no UI slot a story can replace, so the runner focuses the
      -- story card and waits for the player to click it.
      readStory iid attrs Stories.ancientVaultO
      pure $ AncientVaultO $ attrs & setMeta True
    _ -> AncientVaultO <$> liftRunMessage msg attrs
