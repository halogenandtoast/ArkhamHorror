module Arkham.Asset.Cards.Pathfinder1
  ( pathfinder1
  , Pathfinder1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( MoveAction )
import Arkham.Projection

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetCard Pathfinder1
pathfinder1 = asset Pathfinder1 Cards.pathfinder1

instance HasAbilities Pathfinder1 where
  getAbilities (Pathfinder1 attrs) =
    [ restrictedAbility
        attrs
        1
        (ControlsThis <> InvestigatorExists (You <> UnengagedInvestigator))
        (FastAbility $ ExhaustCost (toTarget attrs))
    ]

instance RunMessage Pathfinder1 where
  runMessage msg a@(Pathfinder1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      startId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      accessibleLocationIds <- selectList $ AccessibleFrom $ LocationWithId
        startId
      push $ chooseOne
        iid
        [ targetLabel lid [MoveAction iid lid Free False]
        | lid <- accessibleLocationIds
        ]
      pure a
    _ -> Pathfinder1 <$> runMessage msg attrs
