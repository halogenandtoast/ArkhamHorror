module Arkham.Asset.Cards.StrangeSolutionFreezingVariant4
  ( strangeSolutionFreezingVariant4
  , StrangeSolutionFreezingVariant4(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype StrangeSolutionFreezingVariant4 = StrangeSolutionFreezingVariant4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionFreezingVariant4 :: AssetCard StrangeSolutionFreezingVariant4
strangeSolutionFreezingVariant4 =
  asset StrangeSolutionFreezingVariant4 Cards.strangeSolutionFreezingVariant4

instance HasAbilities StrangeSolutionFreezingVariant4 where
  getAbilities (StrangeSolutionFreezingVariant4 attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Evade)
        $ Costs [ActionCost 1, UseCost (toId attrs) Supply 1]
    ]

instance HasModifiersFor env StrangeSolutionFreezingVariant4 where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Evade)) (InvestigatorTarget iid) (StrangeSolutionFreezingVariant4 a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillAgility 6]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env StrangeSolutionFreezingVariant4 where
  runMessage msg a@(StrangeSolutionFreezingVariant4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push
        (ChooseEvadeEnemy iid source Nothing SkillAgility AnyEnemy False)
    _ -> StrangeSolutionFreezingVariant4 <$> runMessage msg attrs
