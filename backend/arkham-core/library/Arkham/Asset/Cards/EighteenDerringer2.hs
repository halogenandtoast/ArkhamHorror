module Arkham.Asset.Cards.EighteenDerringer2 (
  eighteenDerringer2,
  EighteenDerringer2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Metadata = Metadata {givesBonus :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EighteenDerringer2 = EighteenDerringer2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eighteenDerringer2 :: AssetCard EighteenDerringer2
eighteenDerringer2 =
  asset (EighteenDerringer2 . (`with` Metadata False)) Cards.eighteenDerringer2

instance HasAbilities EighteenDerringer2 where
  getAbilities (EighteenDerringer2 (attrs `With` _)) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId attrs) Ammo 1]
    ]

instance RunMessage EighteenDerringer2 where
  runMessage msg (EighteenDerringer2 (attrs `With` metadata)) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let amount = if givesBonus metadata then 3 else 2
      pushAll
        [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            [DamageDealt 1, SkillModifier SkillCombat amount]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure . EighteenDerringer2 $ attrs `with` Metadata False
    FailedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          pushAll [AddUses (toId attrs) Ammo 1]
          pure . EighteenDerringer2 $ attrs `with` Metadata True
    EndRound -> pure . EighteenDerringer2 $ attrs `with` Metadata False
    _ -> EighteenDerringer2 . (`with` metadata) <$> runMessage msg attrs
