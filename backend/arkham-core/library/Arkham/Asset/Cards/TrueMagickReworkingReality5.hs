module Arkham.Asset.Cards.TrueMagickReworkingReality5 (
  trueMagickReworkingReality5,
  TrueMagickReworkingReality5 (..),
)
where

import Arkham.Ability
import {-# SOURCE #-} Arkham.Asset (createAsset)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (Asset)
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Game.Helpers (getCanPerformAbility)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query (getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Control.Lens (over)
import Data.Data.Lens (biplate)

newtype Metadata = Metadata {currentAsset :: Maybe Asset}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TrueMagickReworkingReality5 = TrueMagickReworkingReality5 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueMagickReworkingReality5 :: AssetCard TrueMagickReworkingReality5
trueMagickReworkingReality5 = asset (TrueMagickReworkingReality5 . (`with` Metadata Nothing)) Cards.trueMagickReworkingReality5

-- This tooltip is handled specially
instance HasAbilities TrueMagickReworkingReality5 where
  getAbilities (TrueMagickReworkingReality5 (With attrs _)) =
    [ withTooltip "Use True Magick"
      $ doesNotProvokeAttacksOfOpportunity
      $ controlledAbility attrs 1 HasTrueMagick aform
    | aform <- [ActionAbility [] mempty, FastAbility Free, freeReaction AnyWindow]
    ]

instance RunMessage TrueMagickReworkingReality5 where
  runMessage msg (TrueMagickReworkingReality5 (With attrs meta)) = runQueueT $ case msg of
    Do BeginRound -> pure . TrueMagickReworkingReality5 . (`with` meta) $ attrs & tokensL . ix Charge %~ max 1
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      hand <- fieldMap InvestigatorHand (filterCards @CardMatcher (#asset <> #spell)) iid
      let adjustCost = overCost (over biplate (const attrs.id))
      choices <- forMaybeM hand \card -> do
        tmpAbilities <-
          filterM
            (getCanPerformAbility iid ws)
            [ adjustCost $ ab {abilitySource = proxy (unsafeMakeCardId $ unAssetId aid) attrs}
            | ab <- getAbilities (createAsset card $ unsafeFromCardId card.id)
            , aid <- maybeToList (preview _AssetSource ab.source)
            ]
        pure $ guard (notNull tmpAbilities) $> (card.id, tmpAbilities)

      player <- getPlayer iid
      chooseOne
        iid
        [ targetLabel cardId [RevealCard cardId, Msg.chooseOne player [AbilityLabel iid a ws [] | a <- as]]
        | (cardId, as) <- choices
        ]

      pure $ TrueMagickReworkingReality5 $ With attrs meta
    UseCardAbility iid (ProxySource (CardIdSource cid) (isSource attrs -> True)) n ws p -> do
      card <- getCard cid
      let iasset = overAttrs (const attrs) (createAsset card attrs.id)
      iasset' <- lift $ runMessage (UseCardAbility iid (toSource attrs) n ws p) iasset
      pure $ TrueMagickReworkingReality5 $ With (toAttrs iasset') (Metadata $ Just iasset')
    ResolvedAbility ab -> do
      case ab.source of
        ProxySource _ (isSource attrs -> True) -> pure $ TrueMagickReworkingReality5 $ With attrs (Metadata Nothing)
        _ -> case currentAsset meta of
          Just iasset -> do
            iasset' <- lift $ runMessage msg iasset
            pure $ TrueMagickReworkingReality5 $ With (toAttrs iasset') (Metadata $ Just iasset')
          Nothing -> TrueMagickReworkingReality5 . (`with` meta) <$> liftRunMessage msg attrs
    _ -> case currentAsset meta of
      Just iasset -> do
        iasset' <- lift $ runMessage msg iasset
        pure $ TrueMagickReworkingReality5 $ With (toAttrs iasset') (Metadata $ Just iasset')
      Nothing -> TrueMagickReworkingReality5 . (`with` meta) <$> liftRunMessage msg attrs
