module Arkham.Asset.Cards.AlchemicalTransmutation
  ( alchemicalTransmutation
  , AlchemicalTransmutation(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype AlchemicalTransmutation = AlchemicalTransmutation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation :: AssetCard AlchemicalTransmutation
alchemicalTransmutation =
  asset AlchemicalTransmutation Cards.alchemicalTransmutation

instance HasAbilities AlchemicalTransmutation where
  getAbilities (AlchemicalTransmutation a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility Nothing $ Costs
        [ExhaustCost (toTarget a), UseCost (AssetWithId $ toId a) Charge 1]
    ]

instance RunMessage AlchemicalTransmutation where
  runMessage msg a@(AlchemicalTransmutation attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ CreateEffect "03032" Nothing source (InvestigatorTarget iid)
      , beginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 1
      ]
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ n -> do
      push $ TakeResources iid (min n 3) (toAbilitySource attrs 1) False
      pure a
    _ -> AlchemicalTransmutation <$> runMessage msg attrs
