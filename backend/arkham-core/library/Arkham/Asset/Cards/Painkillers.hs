module Arkham.Asset.Cards.Painkillers
  ( painkillers
  , Painkillers(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.Matcher hiding ( FastPlayerWindow )

newtype Painkillers = Painkillers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painkillers :: AssetCard Painkillers
painkillers = asset Painkillers Cards.painkillers

instance HasAbilities Painkillers where
  getAbilities (Painkillers a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> InvestigatorExists (HealableInvestigator (toSource a) DamageType You))
        (FastAbility
          (Costs
            [ UseCost (AssetWithId $ toId a) Supply 1
            , ExhaustCost (toTarget a)
            , HorrorCost (toSource a) YouTarget 1
            ]
          )
        )
    ]

instance RunMessage Painkillers where
  runMessage msg a@(Painkillers attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ HealDamage (InvestigatorTarget iid) (toSource attrs) 1
      pure a
    _ -> Painkillers <$> runMessage msg attrs
