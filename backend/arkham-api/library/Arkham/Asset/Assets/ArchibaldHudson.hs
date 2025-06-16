module Arkham.Asset.Assets.ArchibaldHudson (
  archibaldHudson,
  ArchibaldHudson(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype ArchibaldHudson = ArchibaldHudson AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archibaldHudson :: AssetCard ArchibaldHudson
archibaldHudson = allyWith ArchibaldHudson Cards.archibaldHudson (2, 2) noSlots

instance HasAbilities ArchibaldHudson where
  getAbilities (ArchibaldHudson a) =
    [ controlled a 1 (DuringTurn You) $ FastAbility (exhaust a)
    , controlled a 2 ControlsThis $ actionAbilityWithCost (ResourceCost 1)
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
    SearchFound iid (isTarget attrs -> True) _ (EncounterCard ec : _) -> do
      pushAll
        [ InvestigatorDrewEncounterCard iid ec
        , Exhaust (EnemyTarget $ toCardId ec)
        , gainResources iid (attrs.ability 1) 3
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assets <- select $ AssetAt (locationWithInvestigator iid) <> AssetWithDamage
      enemies <- select $ NonEliteEnemy <> enemyAtLocationWith iid
      player <- getPlayer iid
      push
        $ chooseOne player
        $ targetLabels assets \aid ->
            chooseTarget iid enemies \eid ->
              MovedDamage (attrs.ability 2) (toTarget aid) (toTarget eid) 1
      pure a
    _ -> ArchibaldHudson <$> liftRunMessage msg attrs
