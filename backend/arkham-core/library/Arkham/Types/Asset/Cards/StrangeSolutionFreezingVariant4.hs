module Arkham.Types.Asset.Cards.StrangeSolutionFreezingVariant4
  ( strangeSolutionFreezingVariant4
  , StrangeSolutionFreezingVariant4(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype StrangeSolutionFreezingVariant4 = StrangeSolutionFreezingVariant4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionFreezingVariant4 :: AssetCard StrangeSolutionFreezingVariant4
strangeSolutionFreezingVariant4 =
  asset StrangeSolutionFreezingVariant4 Cards.strangeSolutionFreezingVariant4

instance HasActions StrangeSolutionFreezingVariant4 where
  getActions (StrangeSolutionFreezingVariant4 attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Evade)
        $ Costs [ActionCost 1, UseCost (toId attrs) Supply 1]
    ]

instance HasModifiersFor env StrangeSolutionFreezingVariant4 where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Evade)) (InvestigatorTarget iid) (StrangeSolutionFreezingVariant4 a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillAgility 6]
  getModifiersFor _ _ _ = pure []

instance
  ( HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env StrangeSolutionFreezingVariant4 where
  runMessage msg a@(StrangeSolutionFreezingVariant4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push (ChooseEvadeEnemy iid source SkillAgility False)
    _ -> StrangeSolutionFreezingVariant4 <$> runMessage msg attrs
