module Arkham.Asset.Cards.SmokingPipe
  ( smokingPipe
  , SmokingPipe(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Target

newtype SmokingPipe = SmokingPipe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokingPipe :: AssetCard SmokingPipe
smokingPipe = asset SmokingPipe Cards.smokingPipe

instance HasAbilities SmokingPipe where
  getAbilities (SmokingPipe a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> InvestigatorExists (You <> InvestigatorWithAnyHorror))
        (FastAbility
          (Costs
            [ UseCost (AssetWithId $ toId a) Supply 1
            , ExhaustCost (toTarget a)
            , DamageCost (toSource a) YouTarget 1
            ]
          )
        )
    ]

instance RunMessage SmokingPipe where
  runMessage msg a@(SmokingPipe attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealHorror (InvestigatorTarget iid) 1)
    _ -> SmokingPipe <$> runMessage msg attrs
