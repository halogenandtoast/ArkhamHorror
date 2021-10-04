module Arkham.Types.Asset.Cards.Lockpicks1
  ( lockpicks1
  , Lockpicks1(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card.CardDef
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.EffectMetadata
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Lockpicks1 = Lockpicks1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks1 :: AssetCard Lockpicks1
lockpicks1 = hand Lockpicks1 Cards.lockpicks1

instance HasAbilities Lockpicks1 where
  getAbilities (Lockpicks1 a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Investigate)
        $ ExhaustCost
        $ toTarget a
    ]

instance AssetRunner env => RunMessage env Lockpicks1 where
  runMessage msg a@(Lockpicks1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      agility <- getSkillValue SkillAgility iid
      a <$ pushAll
        [ CreateEffect
          (cdCardCode $ toCardDef attrs)
          (Just $ EffectInt agility)
          (toSource attrs)
          (InvestigatorTarget iid)
        , Investigate iid lid source Nothing SkillIntellect False
        ]
    _ -> Lockpicks1 <$> runMessage msg attrs
