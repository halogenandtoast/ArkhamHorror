module Arkham.Asset.Cards.MaskedCarnevaleGoer_21 (
  maskedCarnevaleGoer_21,
  MaskedCarnevaleGoer_21 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Placement

newtype MaskedCarnevaleGoer_21 = MaskedCarnevaleGoer_21 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_21 :: AssetCard MaskedCarnevaleGoer_21
maskedCarnevaleGoer_21 =
  asset MaskedCarnevaleGoer_21 Cards.maskedCarnevaleGoer_21

instance HasAbilities MaskedCarnevaleGoer_21 where
  getAbilities (MaskedCarnevaleGoer_21 x) =
    [ restrictedAbility
        x
        1
        OnSameLocation
        (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost (Static 1)])
    ]

instance RunMessage MaskedCarnevaleGoer_21 where
  runMessage msg a@(MaskedCarnevaleGoer_21 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ Flip iid (toSource iid) (toTarget attrs)
      pure a
    Flip _ _ target | isTarget attrs target -> do
      case assetPlacement attrs of
        AtLocation lid -> do
          let
            innocentReveler = lookupCard Cards.innocentReveler (toCardId attrs)
          pushAll
            [ CreateAssetAt (toId attrs) innocentReveler (AtLocation lid)
            , Flipped (toSource attrs) innocentReveler
            ]
        _ -> error "not possible"
      pure a
    LookAtRevealed _ _ target | isTarget a target -> do
      let
        innocentReveler =
          PlayerCard $ lookupPlayerCard Cards.innocentReveler (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      pushAll
        [ FocusCards [innocentReveler]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_21 <$> runMessage msg attrs
