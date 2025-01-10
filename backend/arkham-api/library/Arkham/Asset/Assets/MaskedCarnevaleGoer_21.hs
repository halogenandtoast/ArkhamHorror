module Arkham.Asset.Assets.MaskedCarnevaleGoer_21 (maskedCarnevaleGoer_21) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Helpers.Query (getLead)
import Arkham.Placement
import Arkham.Projection

newtype MaskedCarnevaleGoer_21 = MaskedCarnevaleGoer_21 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_21 :: AssetCard MaskedCarnevaleGoer_21
maskedCarnevaleGoer_21 =
  asset MaskedCarnevaleGoer_21 Cards.maskedCarnevaleGoer_21

instance HasAbilities MaskedCarnevaleGoer_21 where
  getAbilities (MaskedCarnevaleGoer_21 x) =
    [restricted x 1 OnSameLocation (actionAbilityWithCost $ clueCost 1)]

instance RunMessage MaskedCarnevaleGoer_21 where
  runMessage msg a@(MaskedCarnevaleGoer_21 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let innocentReveler = lookupCard Cards.innocentReveler (toCardId attrs)
      pushAll
        [ CreateAssetAt (toId attrs) innocentReveler (AtLocation location)
        , Flipped (toSource attrs) innocentReveler
        ]
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let innocentReveler = PlayerCard $ lookupPlayerCard Cards.innocentReveler (toCardId attrs)
      lead <- getLead
      focusCard innocentReveler $ continue_ lead
      pure a
    _ -> MaskedCarnevaleGoer_21 <$> liftRunMessage msg attrs
