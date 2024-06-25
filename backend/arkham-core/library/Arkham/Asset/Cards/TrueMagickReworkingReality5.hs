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
import Arkham.Trait (Trait (Spell))
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

instance HasAbilities TrueMagickReworkingReality5 where
  getAbilities (TrueMagickReworkingReality5 (With attrs _)) =
    [ doesNotProvokeAttacksOfOpportunity
        $ (controlledAbility attrs 1 HasTrueMagick $ ActionAbility [] mempty)
          { abilityDisplayAsAction = False
          , abilityTooltip = Just "Use True Magick"
          }
    , doesNotProvokeAttacksOfOpportunity
        $ (controlledAbility attrs 1 HasTrueMagick $ FastAbility Free)
          { abilityDisplayAsAction = False
          , abilityTooltip = Just "Use True Magick"
          }
    , doesNotProvokeAttacksOfOpportunity
        $ (controlledAbility attrs 1 HasTrueMagick $ ReactionAbility AnyWindow Free)
          { abilityDisplayAsAction = False
          , abilityTooltip = Just "Use True Magick"
          }
    ]

instance RunMessage TrueMagickReworkingReality5 where
  runMessage msg (TrueMagickReworkingReality5 (With attrs meta)) = runQueueT $ case msg of
    Do BeginRound -> pure . TrueMagickReworkingReality5 . (`with` meta) $ attrs & usesL . ix Charge %~ max 1
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      hand <-
        fieldMap InvestigatorHand (filter (`cardMatch` (CardWithType AssetType <> CardWithTrait Spell))) iid
      let
        replaceAssetId = const attrs.id
        replaceAssetIds = over biplate replaceAssetId
      choices <- forMaybeM hand \card -> do
        let tmpAsset = createAsset card (AssetId $ unsafeCardIdToUUID card.id)
            tmpAbilities =
              map
                ( \ab -> case ab.source of
                    AssetSource aid ->
                      overCost replaceAssetIds
                        $ ab {abilitySource = proxy (CardIdSource $ unsafeMakeCardId $ unAssetId aid) attrs}
                    _ -> error "wrong source"
                )
                (getAbilities tmpAsset)
        tmpAbilities' <- filterM (getCanPerformAbility iid ws) tmpAbilities
        pure $ guard (notNull tmpAbilities') $> (card.id, tmpAbilities')

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
        ProxySource (isSource attrs -> True) _ -> pure $ TrueMagickReworkingReality5 $ With attrs (Metadata Nothing)
        _ -> case currentAsset meta of
          Just iasset -> do
            iasset' <- lift $ runMessage msg iasset
            pure $ TrueMagickReworkingReality5 $ With (toAttrs iasset') (Metadata $ Just iasset')
          Nothing -> TrueMagickReworkingReality5 . (`with` meta) <$> lift (runMessage msg attrs)
    _ -> case currentAsset meta of
      Just iasset -> do
        iasset' <- lift $ runMessage msg iasset
        pure $ TrueMagickReworkingReality5 $ With (toAttrs iasset') (Metadata $ Just iasset')
      Nothing -> TrueMagickReworkingReality5 . (`with` meta) <$> lift (runMessage msg attrs)
