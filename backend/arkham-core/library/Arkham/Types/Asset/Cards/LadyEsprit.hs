module Arkham.Types.Asset.Cards.LadyEsprit
  ( LadyEsprit(..)
  , ladyEsprit
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Target

newtype LadyEsprit = LadyEsprit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ladyEsprit :: AssetCard LadyEsprit
ladyEsprit = allyWith LadyEsprit Cards.ladyEsprit (2, 4) (isStoryL .~ True)

instance HasAbilities LadyEsprit where
  getAbilities (LadyEsprit x) =
    [ restrictedAbility
        x
        1
        OnSameLocation
        (ActionAbility Nothing $ Costs
          [ ActionCost 1
          , ExhaustCost (toTarget x)
          , HorrorCost (toSource x) (toTarget x) 1
          ]
        )
    ]

instance AssetRunner env => RunMessage env LadyEsprit where
  runMessage msg a@(LadyEsprit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push
        (chooseOne
          iid
          [HealDamage (InvestigatorTarget iid) 2, TakeResources iid 2 False]
        )
    _ -> LadyEsprit <$> runMessage msg attrs
