module Arkham.Asset.Cards.StrangeSolutionAcidicIchor4
  ( strangeSolutionAcidicIchor4
  , StrangeSolutionAcidicIchor4(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype StrangeSolutionAcidicIchor4 = StrangeSolutionAcidicIchor4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionAcidicIchor4 :: AssetCard StrangeSolutionAcidicIchor4
strangeSolutionAcidicIchor4 =
  asset StrangeSolutionAcidicIchor4 Cards.strangeSolutionAcidicIchor4

instance HasAbilities StrangeSolutionAcidicIchor4 where
  getAbilities (StrangeSolutionAcidicIchor4 attrs) =
    [ restrictedAbility attrs 1 OwnsThis
      $ ActionAbility (Just Action.Fight)
      $ Costs [ActionCost 1, UseCost (AssetWithId $ toId attrs) Supply 1]
    ]

instance HasModifiersFor env StrangeSolutionAcidicIchor4 where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) (InvestigatorTarget iid) (StrangeSolutionAcidicIchor4 a)
    | controlledBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillCombat 6, DamageDealt 2]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage StrangeSolutionAcidicIchor4 where
  runMessage msg a@(StrangeSolutionAcidicIchor4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push (ChooseFightEnemy iid source Nothing SkillCombat mempty False)
    _ -> StrangeSolutionAcidicIchor4 <$> runMessage msg attrs
