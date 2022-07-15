module Arkham.Asset.Cards.Scrying
  ( Scrying(..)
  , scrying
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Zone

newtype Scrying = Scrying AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrying :: AssetCard Scrying
scrying = asset Scrying Cards.scrying

instance HasAbilities Scrying where
  getAbilities (Scrying a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility Nothing $ Costs
        [ ActionCost 1
        , UseCost (AssetWithId $ toId a) Charge 1
        , ExhaustCost $ toTarget a
        ]
    ]

instance RunMessage Scrying where
  runMessage msg a@(Scrying attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- map InvestigatorTarget <$> getInvestigatorIds
      push $ chooseOne
        iid
        [ TargetLabel
            target
            [ Search
                iid
                source
                target
                [(FromTopOfDeck 3, PutBackInAnyOrder)]
                AnyCard
                ReturnCards
            ]
        | target <- (EncounterDeckTarget : targets)
        ]
      pure a
    _ -> Scrying <$> runMessage msg attrs
