module Arkham.Types.Asset.Cards.Rolands38Special
  ( Rolands38Special(..)
  , rolands38Special
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Rolands38Special = Rolands38Special AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolands38Special :: AssetCard Rolands38Special
rolands38Special = hand Rolands38Special Cards.rolands38Special

instance HasAbilities Rolands38Special where
  getAbilities (Rolands38Special x) =
    [ restrictedAbility x 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (toId x) Ammo 1])
    ]

instance AssetRunner env => RunMessage env Rolands38Special where
  runMessage msg a@(Rolands38Special attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      anyClues <- (> 0) . unClueCount <$> getCount locationId
      a <$ pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 1, SkillModifier SkillCombat (if anyClues then 3 else 1)]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> Rolands38Special <$> runMessage msg attrs
