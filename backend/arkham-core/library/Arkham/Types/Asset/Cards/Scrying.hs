module Arkham.Types.Asset.Cards.Scrying
  ( Scrying(..)
  , scrying
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype Scrying = Scrying AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

scrying :: AssetCard Scrying
scrying = arcane Scrying Cards.scrying

instance HasModifiersFor env Scrying

instance HasActions Scrying where
  getActions (Scrying a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, UseCost (toId a) Charge 1, ExhaustThis]
    ]

instance AssetRunner env => RunMessage env Scrying where
  runMessage msg a@(Scrying attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      targets <- map InvestigatorTarget <$> getSetList locationId
      a <$ push
        (chooseOne iid
        $ SearchTopOfDeck
            iid
            source
            EncounterDeckTarget
            3
            []
            PutBackInAnyOrder
        : [ SearchTopOfDeck iid source target 3 [] PutBackInAnyOrder
          | target <- targets
          ]
        )
    _ -> Scrying <$> runMessage msg attrs
