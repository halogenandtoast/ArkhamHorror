module Arkham.Asset.Cards.MaskedCarnevaleGoer_21
  ( maskedCarnevaleGoer_21
  , MaskedCarnevaleGoer_21(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Cost
import Arkham.Criteria
import Arkham.Placement
import Arkham.Source

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
        (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
    ]

instance RunMessage MaskedCarnevaleGoer_21 where
  runMessage msg a@(MaskedCarnevaleGoer_21 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ push (Flip iid (InvestigatorSource iid) (toTarget attrs))
    Flip _ _ target | isTarget attrs target -> do
      case assetPlacement attrs of
        AtLocation lid -> do
          let
            innocentReveler = PlayerCard
              $ lookupPlayerCard Cards.innocentReveler (toCardId attrs)
          a <$ pushAll
            [ CreateAssetAt innocentReveler (AtLocation lid)
            , Flipped (toSource attrs) innocentReveler
            ]
        _ -> error "not possible"
    LookAtRevealed _ _ target | isTarget a target -> do
      let
        innocentReveler =
          PlayerCard $ lookupPlayerCard Cards.innocentReveler (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [innocentReveler]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
    _ -> MaskedCarnevaleGoer_21 <$> runMessage msg attrs
