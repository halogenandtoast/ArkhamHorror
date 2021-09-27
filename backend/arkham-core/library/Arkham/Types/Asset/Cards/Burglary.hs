module Arkham.Types.Asset.Cards.Burglary
  ( Burglary(..)
  , burglary
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.SkillType

newtype Burglary = Burglary AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burglary :: AssetCard Burglary
burglary = asset Burglary Cards.burglary

instance HasAbilities Burglary where
  getAbilities (Burglary a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Investigate)
        $ Costs [ActionCost 1, ExhaustCost (toTarget a)]
    ]

instance AssetRunner env => RunMessage env Burglary where
  runMessage msg a@(Burglary attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      a
        <$ push
             (Investigate
               iid
               lid
               source
               (Just $ toTarget attrs)
               SkillIntellect
               False
             )
    Successful (Action.Investigate, _) iid _ target | isTarget attrs target ->
      a <$ pushAll [TakeResources iid 3 False]
    _ -> Burglary <$> runMessage msg attrs
