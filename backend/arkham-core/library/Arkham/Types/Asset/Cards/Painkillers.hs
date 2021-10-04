module Arkham.Types.Asset.Cards.Painkillers
  ( painkillers
  , Painkillers(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher hiding (FastPlayerWindow)
import Arkham.Types.Target

newtype Painkillers = Painkillers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painkillers :: AssetCard Painkillers
painkillers = asset Painkillers Cards.painkillers

instance HasAbilities Painkillers where
  getAbilities (Painkillers a) =
    [ restrictedAbility
        a
        1
        (OwnsThis <> InvestigatorExists (You <> InvestigatorWithAnyDamage))
        (FastAbility
          (Costs
            [ UseCost (toId a) Supply 1
            , ExhaustCost (toTarget a)
            , HorrorCost (toSource a) YouTarget 1
            ]
          )
        )
    ]

instance AssetRunner env => RunMessage env Painkillers where
  runMessage msg a@(Painkillers attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealDamage (InvestigatorTarget iid) 1)
    _ -> Painkillers <$> runMessage msg attrs
