module Arkham.Types.Asset.Cards.SmokingPipe
  ( smokingPipe
  , SmokingPipe(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype SmokingPipe = SmokingPipe AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokingPipe :: AssetCard SmokingPipe
smokingPipe = asset SmokingPipe Cards.smokingPipe

instance HasActions SmokingPipe where
  getActions (SmokingPipe a) =
    [ restrictedAbility
        a
        1
        (OwnsThis <> InvestigatorExists (You <> InvestigatorWithHorror))
        (FastAbility
          (Costs
            [ UseCost (toId a) Supply 1
            , ExhaustCost (toTarget a)
            , DamageCost (toSource a) YouTarget 1
            ]
          )
        )
    ]

instance HasModifiersFor env SmokingPipe

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SmokingPipe where
  runMessage msg a@(SmokingPipe attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealHorror (InvestigatorTarget iid) 1)
    _ -> SmokingPipe <$> runMessage msg attrs
