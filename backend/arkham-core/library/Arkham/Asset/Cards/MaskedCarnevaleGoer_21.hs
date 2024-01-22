module Arkham.Asset.Cards.MaskedCarnevaleGoer_21 (maskedCarnevaleGoer_21, MaskedCarnevaleGoer_21 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection

newtype MaskedCarnevaleGoer_21 = MaskedCarnevaleGoer_21 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_21 :: AssetCard MaskedCarnevaleGoer_21
maskedCarnevaleGoer_21 =
  asset MaskedCarnevaleGoer_21 Cards.maskedCarnevaleGoer_21

instance HasAbilities MaskedCarnevaleGoer_21 where
  getAbilities (MaskedCarnevaleGoer_21 x) =
    [ restrictedAbility x 1 OnSameLocation (actionAbilityWithCost $ ClueCost (Static 1))
    ]

instance RunMessage MaskedCarnevaleGoer_21 where
  runMessage msg a@(MaskedCarnevaleGoer_21 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Flip iid (toSource iid) (toTarget attrs)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let innocentReveler = lookupCard Cards.innocentReveler (toCardId attrs)
      pushAll
        [ CreateAssetAt (toId attrs) innocentReveler (AtLocation location)
        , Flipped (toSource attrs) innocentReveler
        ]
      pure a
    LookAtRevealed _ _ target | isTarget a target -> do
      let innocentReveler = PlayerCard $ lookupPlayerCard Cards.innocentReveler (toCardId attrs)
      lead <- getLeadPlayer
      pushAll [FocusCards [innocentReveler], chooseOne lead [Label "Continue" [UnfocusCards]]]
      pure a
    _ -> MaskedCarnevaleGoer_21 <$> runMessage msg attrs
