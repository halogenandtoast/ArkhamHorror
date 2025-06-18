module Arkham.Asset.Assets.ArchibaldHudson (archibaldHudson) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype ArchibaldHudson = ArchibaldHudson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archibaldHudson :: AssetCard ArchibaldHudson
archibaldHudson = allyWith ArchibaldHudson Cards.archibaldHudson (2, 2) noSlots

instance HasAbilities ArchibaldHudson where
  getAbilities (ArchibaldHudson a) =
    [ controlled a 1 (DuringTurn You) $ FastAbility (exhaust a)
    , restricted a 2 ControlsThis $ actionAbilityWithCost (ResourceCost 1)
    ]

instance RunMessage ArchibaldHudson where
  runMessage msg a@(ArchibaldHudson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        EncounterDeckTarget
        [fromTopOfDeck 9]
        (basic #enemy)
        (defer attrs IsDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ (ec : _) -> do
      drawCard iid ec
      forTarget ec msg
      gainResources iid (attrs.ability 1) 3
      pure a
    ForTarget (CardIdTarget cid) (SearchFound _iid (isTarget attrs -> True) _ _) -> do
      selectEach (EnemyWithCardId cid) exhaustThis
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assets <- select $ AssetAt (locationWithInvestigator iid) <> AssetWithDamage
      enemies <- select $ NonEliteEnemy <> enemyAtLocationWith iid
      chooseOneM iid do
        targets assets \aid ->
          chooseTargetM iid enemies \eid ->
            moveTokens (attrs.ability 2) aid eid #damage 1
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ ArchibaldHudson $ attrs & flippedL .~ True & visibleL .~ False
    _ -> ArchibaldHudson <$> liftRunMessage msg attrs
