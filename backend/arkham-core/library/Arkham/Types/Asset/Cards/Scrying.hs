module Arkham.Types.Asset.Cards.Scrying
  ( Scrying(..)
  , scrying
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Zone

newtype Scrying = Scrying AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

scrying :: AssetCard Scrying
scrying = arcane Scrying Cards.scrying

instance HasAbilities Scrying where
  getAbilities (Scrying a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, UseCost (toId a) Charge 1, ExhaustCost $ toTarget a]
    ]

instance AssetRunner env => RunMessage env Scrying where
  runMessage msg a@(Scrying attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- map InvestigatorTarget <$> getInvestigatorIds
      a <$ push
        (chooseOne iid
        $ Search
            iid
            source
            EncounterDeckTarget
            (FromTopOfDeck 3)
            []
            PutBackInAnyOrder
        : [ Search iid source target (FromTopOfDeck 3) [] PutBackInAnyOrder
          | target <- targets
          ]
        )
    _ -> Scrying <$> runMessage msg attrs
