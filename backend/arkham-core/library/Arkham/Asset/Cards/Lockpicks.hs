module Arkham.Asset.Cards.Lockpicks
  ( lockpicks
  , Lockpicks(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card.CardDef
import Arkham.Cost
import Arkham.Criteria
import Arkham.EffectMetadata
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype Lockpicks = Lockpicks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks :: AssetCard Lockpicks
lockpicks = asset Lockpicks Cards.lockpicks

instance HasAbilities Lockpicks where
  getAbilities (Lockpicks a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ ExhaustCost
        $ toTarget a
    ]

instance RunMessage Lockpicks where
  runMessage msg a@(Lockpicks attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      lid <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      agility <- getSkillValue SkillAgility iid
      a <$ pushAll
        [ CreateEffect
          (cdCardCode $ toCardDef attrs)
          (Just $ EffectInt agility)
          (toSource attrs)
          (InvestigatorTarget iid)
        , Investigate iid lid source Nothing SkillIntellect False
        ]
    _ -> Lockpicks <$> runMessage msg attrs
