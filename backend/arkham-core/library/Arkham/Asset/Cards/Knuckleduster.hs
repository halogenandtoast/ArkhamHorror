module Arkham.Asset.Cards.Knuckleduster
  ( knuckleduster
  , Knuckleduster(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Keyword qualified as Keyword
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype Knuckleduster = Knuckleduster AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knuckleduster :: AssetCard Knuckleduster
knuckleduster = asset Knuckleduster Cards.knuckleduster

instance HasAbilities Knuckleduster where
  getAbilities (Knuckleduster a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance HasModifiersFor Knuckleduster where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) (EnemyTarget eid) (Knuckleduster attrs)
    | isSource attrs source
    = do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just (EnemyTarget eid') | eid == eid' -> pure $ toModifiers attrs [AddKeyword Keyword.Retaliate]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage Knuckleduster where
  runMessage msg a@(Knuckleduster attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ pushAll
        [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> Knuckleduster <$> runMessage msg attrs
