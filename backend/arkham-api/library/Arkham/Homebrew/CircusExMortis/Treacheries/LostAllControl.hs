module Arkham.Homebrew.CircusExMortis.Treacheries.LostAllControl (lostAllControl) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n, moonToken)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Ally))
import Arkham.Treachery.Import.Lifted

newtype LostAllControl = LostAllControl TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostAllControl :: TreacheryCard LostAllControl
lostAllControl = treachery LostAllControl Cards.lostAllControl

abilitiesOnAttached :: TreacheryAttrs -> Maybe AbilityMatcher
abilitiesOnAttached a = case a.placement of
  AttachedToAsset aid _ -> Just $ AbilityOnAsset (AssetWithId aid)
  AttachedToInvestigator iid -> Just $ AbilityOnInvestigator (InvestigatorWithId iid)
  _ -> Nothing

attachedLocation :: TreacheryAttrs -> Maybe LocationMatcher
attachedLocation a = case a.placement of
  AttachedToAsset aid _ -> Just $ locationWithAsset aid
  AttachedToInvestigator iid -> Just $ locationWithInvestigator iid
  _ -> Nothing

instance HasAbilities LostAllControl where
  getAbilities (LostAllControl a) =
    [ mkAbility a 1 $ forced $ ActivateAbility #after Anyone matcher
    | matcher <- toList (abilitiesOnAttached a)
    ]
      <> [ mkAbility a 2 $ freeReaction $ ChaosTokenReleased #after (InvestigatorAt lmatcher) moonToken
         | lmatcher <- toList (attachedLocation a)
         ]

instance RunMessage LostAllControl where
  runMessage msg t@(LostAllControl attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allies <-
        select
          $ AssetWithTrait Ally
          <> AssetControlledBy Anyone
          <> not_ (AssetWithAttachedTreachery $ treacheryIs Cards.lostAllControl)
      copies <- select $ treacheryIs Cards.lostAllControl
      taken <- traverse (field TreacheryPlacement) copies
      let untaken i = AttachedToInvestigator i `notElem` taken
      investigators <- filter untaken <$> select Anyone
      assetHasAbility <- filterM (fmap notNull . select . AbilityOnAsset . AssetWithId) allies
      investigatorHasAbility <-
        filterM (fmap notNull . select . AbilityOnInvestigator . InvestigatorWithId) investigators
      let (assets', investigators') =
            if null assetHasAbility && null investigatorHasAbility
              then (allies, investigators)
              else (assetHasAbility, investigatorHasAbility)
      chooseOrRunOneM iid do
        for_ assets' \aid -> targeting aid $ place attrs (AttachedToAsset aid Nothing)
        for_ investigators' \i -> targeting i $ place attrs (AttachedToInvestigator i)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ campaignI18n $ scope "lostAllControl" $ case attrs.placement of
        AttachedToAsset aid _ -> do
          labeled "directDamage" $ dealAssetDirectDamage aid attrs 1
          labeled "directHorror" $ dealAssetDirectDamageAndHorror aid attrs 0 1
        AttachedToInvestigator bearer -> do
          labeled "directDamage" $ directDamageAndHorror bearer attrs 1 0
          labeled "directHorror" $ directDamageAndHorror bearer attrs 0 1
        _ -> pure ()
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> LostAllControl <$> liftRunMessage msg attrs
