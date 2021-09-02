module Arkham.Types.Asset.Cards.MaskedCarnevaleGoer_21
  ( maskedCarnevaleGoer_21
  , MaskedCarnevaleGoer_21(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Source

newtype MaskedCarnevaleGoer_21 = MaskedCarnevaleGoer_21 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

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

instance AssetRunner env => RunMessage env MaskedCarnevaleGoer_21 where
  runMessage msg a@(MaskedCarnevaleGoer_21 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push (Flip (InvestigatorSource iid) (toTarget attrs))
    Flip _ target | isTarget attrs target -> do
      case assetLocation attrs of
        Just lid -> do
          let
            innocentReveler = PlayerCard
              $ lookupPlayerCard Cards.innocentReveler (toCardId attrs)
          a <$ pushAll
            [ RemoveFromGame (toTarget attrs)
            , CreateStoryAssetAt innocentReveler lid
            ]
        Nothing -> error "not possible"
    LookAtRevealed _ target | isTarget a target -> do
      let
        innocentReveler =
          PlayerCard $ lookupPlayerCard Cards.innocentReveler (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [innocentReveler]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
    _ -> MaskedCarnevaleGoer_21 <$> runMessage msg attrs
