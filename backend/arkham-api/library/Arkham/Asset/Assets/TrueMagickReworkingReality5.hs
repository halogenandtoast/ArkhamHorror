module Arkham.Asset.Assets.TrueMagickReworkingReality5 (trueMagickReworkingReality5) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.Asset (createAsset)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (createAsset)
import Arkham.Asset.Types (Asset (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Constants
import {-# SOURCE #-} Arkham.Entities
import {-# SOURCE #-} Arkham.Game
import Arkham.GameEnv
import Arkham.Helpers.Ability (getCanPerformAbility)
import Arkham.Helpers.Criteria (getTrueMagickGrantedTraits)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (getWindowRevealedCardId)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Control.Lens (over)
import Control.Monad.Reader (local)
import Control.Monad.Writer.Strict (execWriterT)
import Data.Data.Lens (biplate)
import Data.Map.Monoidal.Strict (getMonoidalMap)

newtype Metadata = Metadata {currentAsset :: Maybe Asset}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TrueMagickReworkingReality5 = TrueMagickReworkingReality5 (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueMagickReworkingReality5 :: AssetCard TrueMagickReworkingReality5
trueMagickReworkingReality5 = asset (TrueMagickReworkingReality5 . (`with` Metadata Nothing)) Cards.trueMagickReworkingReality5

{- | The metadata asset wears a copy of True Magick's attrs under the borrowed
card's code, and @With@'s Entity instance reads @toAttrs@ from the outer half
alone. Anything the borrowed ability writes to its attrs -- the charge Second
Sight spends, an exhaust, damage soaked -- therefore has to be carried back out,
or it dies with the metadata. #5801
-}
toInner :: AssetAttrs -> Asset -> Asset
toInner attrs = overAttrs \i -> attrs {assetCardCode = assetCardCode i}

fromInner :: AssetAttrs -> Asset -> AssetAttrs
fromInner attrs i = (toAttrs i) {assetCardCode = assetCardCode attrs}

instance HasModifiersFor TrueMagickReworkingReality5 where
  getModifiersFor (TrueMagickReworkingReality5 (a `With` meta)) = do
    case currentAsset meta of
      -- Mid-resolution: True Magick IS the revealed asset, so copy its traits
      -- exactly (e.g. so Twila / "Spell" triggers see the borrowed asset).
      Just b -> do
        let selfTraits = cdCardTraits (toCardDef a)
        let otherTraits = cdCardTraits (toCardDef b)
        let removals = toList $ selfTraits `difference` otherTraits
        let additions = toList $ otherTraits `difference` selfTraits
        modifySelf a $ map AddTrait additions <> map RemoveTrait removals
      -- At rest: read as Spell/Ritual ONLY when there is a castable in-hand
      -- [Spell] asset to borrow, so Sign Magick (3)'s hasAnyTrait [Spell, Ritual]
      -- criterion can target True Magick. Restricted to Spell/Ritual to avoid
      -- True Magick being swept up by "all your Spell assets" effects at rest.
      Nothing -> do
        traits <- getTrueMagickGrantedTraits a
        modifySelf a $ map AddTrait traits

-- This tooltip is handled specially
--
-- NonActivateAbility (not index 1): this ability is only a wrapper that picks
-- which in-hand spell ability to resolve -- the borrowed ability, re-sourced via
-- ProxySource below, is the one real activation. At a normal index ActiveCost
-- would account for the wrapper as a second activate action of its own (its own
-- ActivateAbility/PerformAction windows, FinishAction and TakenActions), so
-- Sign Magick (3) asked twice and Haste (2) saw two Activates in a row (#5298).
-- Same idiom as Tony Morgan's Bounty action.
instance HasAbilities TrueMagickReworkingReality5 where
  getAbilities (TrueMagickReworkingReality5 (With attrs (Metadata Nothing))) =
    [ (cardI18n $ withI18nTooltip "trueMagickReworkingReality5.useTrueMagick")
        $ doesNotProvokeAttacksOfOpportunity
        $ controlled attrs NonActivateAbility HasTrueMagick aform
    | aform <- [ActionAbility mempty Nothing mempty, FastAbility Free, freeReaction AnyWindow]
    ]
  getAbilities (TrueMagickReworkingReality5 (With _ (Metadata (Just inner)))) = getAbilities inner

instance RunMessage TrueMagickReworkingReality5 where
  runMessage msg (TrueMagickReworkingReality5 (With attrs meta)) = runQueueT $ case msg of
    Do BeginRound -> do
      pure . TrueMagickReworkingReality5 . (`with` meta) $ attrs & tokensL %~ replenish #charge 1
    UseCardAbility iid (isSource attrs -> True) NonActivateAbility ws _ -> do
      -- A borrowed activation IS the revealed asset -- True Magick becomes a copy of it,
      -- name included (FAQ v2.5 Q69) -- so a [Spell] already revealed in this chain is
      -- the SAME asset and must not be offered again when a trigger points back at us.
      -- Sign Magick (3) forwards its ActivateAbility window, which carries that card id.
      -- That window is the trigger, not a window the borrowed ability may be used in, so
      -- it is stripped before the performability check.
      let revealed = mapMaybe getWindowRevealedCardId ws
      let ws' = filter (isNothing . getWindowRevealedCardId) ws
      hand <-
        fieldMap
          InvestigatorHand
          (filter ((`notElem` revealed) . toCardId) . filterCards (card_ $ #asset <> #spell))
          iid
      let adjustCost = overCost (over biplate (const attrs.id))
      choices <- forMaybeM hand \card -> do
        let a =
              overAttrs (\attrs' -> attrs {assetCardCode = assetCardCode attrs'})
                $ createAsset card
                $ unsafeFromCardId card.id
        tmpAbilities <-
          getGame >>= runReaderT do
            local (entitiesL %~ addEntity a) do
              modifiers <- getMonoidalMap <$> execWriterT (getModifiersFor a)
              local (modifiersL <>~ modifiers) do
                filterM (getCanPerformAbility iid ws') [adjustCost ab | ab <- getAbilities a]
        pure $ guard (notNull tmpAbilities) $> (card.id, tmpAbilities)

      player <- getPlayer iid
      -- Never ask with nothing to offer: `chooseOne` on an empty list is an `error`.
      -- `AssetWithPerformableAbility` (the matcher Sign Magick (3)'s criterion goes
      -- through) checks us against `defaultWindows`, so it cannot see the revealed-card
      -- exclusion above and may offer the reaction when the only in-hand [Spell] is the
      -- one already revealed. #5801
      unless (null choices) do
        chooseOne
          iid
          [ targetLabel
              cardId
              [ RevealCard cardId
              , Msg.chooseOne
                  player
                  [AbilityLabel iid a {abilitySource = proxy (CardIdSource cardId) attrs} ws [] [] | a <- as]
              ]
          | (cardId, as) <- choices
          ]

      pure $ TrueMagickReworkingReality5 $ With attrs meta
    UseCardAbility iid (ProxySource (CardIdSource cid) (isSource attrs -> True)) n ws p -> do
      card <- getCard cid
      assetId <- getRandom
      let iasset = overAttrs (const (attrs {assetCardCode = card.cardCode})) (createAsset card assetId)
      iasset' <- lift $ runMessage (UseCardAbility iid (toSource attrs) n ws p) iasset
      pure $ TrueMagickReworkingReality5 $ With (fromInner attrs iasset') (Metadata $ Just iasset')
    ResolvedAbility ab -> do
      case ab.source of
        ProxySource _ (isSource attrs -> True) ->
          pure $ TrueMagickReworkingReality5 $ With attrs (Metadata Nothing)
        _ -> case currentAsset meta of
          Just iasset -> do
            iasset' <- lift $ runMessage msg (toInner attrs iasset)
            pure
              $ TrueMagickReworkingReality5
              $ With (fromInner attrs iasset') (Metadata $ Just iasset')
          Nothing -> TrueMagickReworkingReality5 . (`with` meta) <$> liftRunMessage msg attrs
    _ -> case currentAsset meta of
      Just iasset -> do
        iasset' <- lift $ runMessage msg (toInner attrs iasset)
        pure $ TrueMagickReworkingReality5 $ With (fromInner attrs iasset') (Metadata $ Just iasset')
      Nothing -> TrueMagickReworkingReality5 . (`with` meta) <$> liftRunMessage msg attrs
