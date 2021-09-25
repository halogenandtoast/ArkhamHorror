module Arkham.Types.Asset.Cards.Knuckleduster
  ( knuckleduster
  , Knuckleduster(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype Knuckleduster = Knuckleduster AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knuckleduster :: AssetCard Knuckleduster
knuckleduster = asset Knuckleduster Cards.knuckleduster

instance HasAbilities Knuckleduster where
  getAbilities (Knuckleduster a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance HasModifiersFor env Knuckleduster where
  getModifiersFor (SkillTestSource _ _ source (EnemyTarget eid) (Just Action.Fight)) (EnemyTarget eid') (Knuckleduster attrs)
    | isSource attrs source && eid == eid'
    = do
      pure $ toModifiers attrs [AddKeyword Keyword.Retaliate]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env Knuckleduster where
  runMessage msg a@(Knuckleduster attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ pushAll
        [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> Knuckleduster <$> runMessage msg attrs
