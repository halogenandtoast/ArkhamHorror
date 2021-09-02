module Arkham.Types.Asset.Cards.SmokingPipe
  ( smokingPipe
  , SmokingPipe(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher hiding (FastPlayerWindow)
import Arkham.Types.Message
import Arkham.Types.Target

newtype SmokingPipe = SmokingPipe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokingPipe :: AssetCard SmokingPipe
smokingPipe = asset SmokingPipe Cards.smokingPipe

instance HasAbilities SmokingPipe where
  getAbilities (SmokingPipe a) =
    [ restrictedAbility
        a
        1
        (OwnsThis <> InvestigatorExists (You <> InvestigatorWithAnyHorror))
        (FastAbility
          (Costs
            [ UseCost (toId a) Supply 1
            , ExhaustCost (toTarget a)
            , DamageCost (toSource a) YouTarget 1
            ]
          )
        )
    ]

instance AssetRunner env => RunMessage env SmokingPipe where
  runMessage msg a@(SmokingPipe attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealHorror (InvestigatorTarget iid) 1)
    _ -> SmokingPipe <$> runMessage msg attrs
