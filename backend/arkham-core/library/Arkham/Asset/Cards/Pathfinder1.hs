module Arkham.Asset.Cards.Pathfinder1
  ( pathfinder1
  , Pathfinder1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher hiding (MoveAction)
import Arkham.Target

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetCard Pathfinder1
pathfinder1 = asset Pathfinder1 Cards.pathfinder1

instance HasAbilities Pathfinder1 where
  getAbilities (Pathfinder1 attrs) =
    [ restrictedAbility
        attrs
        1
        (OwnsThis <> InvestigatorExists (You <> UnengagedInvestigator))
        (FastAbility $ ExhaustCost (toTarget attrs))
    ]

instance AssetRunner env => RunMessage env Pathfinder1 where
  runMessage msg a@(Pathfinder1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      accessibleLocationIds <-
        map unAccessibleLocationId <$> (getSetList =<< getId @LocationId iid)
      a <$ push
        (chooseOne
          iid
          [ TargetLabel (LocationTarget lid) [MoveAction iid lid Free False]
          | lid <- accessibleLocationIds
          ]
        )
    _ -> Pathfinder1 <$> runMessage msg attrs
