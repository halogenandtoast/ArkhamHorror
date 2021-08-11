module Arkham.Types.Asset.Cards.LadyEsprit
  ( LadyEsprit(..)
  , ladyEsprit
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype LadyEsprit = LadyEsprit AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ladyEsprit :: AssetCard LadyEsprit
ladyEsprit = allyWith LadyEsprit Cards.ladyEsprit (2, 4) (isStoryL .~ True)

instance HasModifiersFor env LadyEsprit

instance HasActions LadyEsprit where
  getActions (LadyEsprit x) =
    [ restrictedAbility
        x
        1
        OnSameLocation
        (ActionAbility Nothing $ Costs
          [ActionCost 1, ExhaustThis, HorrorCost (toSource x) (toTarget x) 1]
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
