module Arkham.Asset.Cards.SmokingPipe (
  smokingPipe,
  SmokingPipe (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (FastPlayerWindow)

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
        ( ControlsThis
            <> InvestigatorExists
              (HealableInvestigator (toSource a) HorrorType You)
        )
        ( FastAbility
            ( Costs
                [ UseCost (AssetWithId $ toId a) Supply 1
                , ExhaustCost (toTarget a)
                , DamageCost (toSource a) YouTarget 1
                ]
            )
        )
    ]

instance RunMessage SmokingPipe where
  runMessage msg a@(SmokingPipe attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      pushWhen canHeal $ HealHorror (toTarget iid) (attrs.ability 1) 1
      pure a
    _ -> SmokingPipe <$> runMessage msg attrs
