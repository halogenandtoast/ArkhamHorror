module Arkham.Asset.Cards.FortyOneDerringer2 (
  fortyOneDerringer2,
  FortyOneDerringer2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Metadata = Metadata {gotExtraAction :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks)

newtype FortyOneDerringer2 = FortyOneDerringer2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

fortyOneDerringer2 :: AssetCard FortyOneDerringer2
fortyOneDerringer2 =
  asset (FortyOneDerringer2 . (`with` Metadata False)) Cards.fortyOneDerringer2

instance HasAbilities FortyOneDerringer2 where
  getAbilities (FortyOneDerringer2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility
          ([Action.Fight])
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1])
    ]

instance RunMessage FortyOneDerringer2 where
  runMessage msg a@(FortyOneDerringer2 (attrs `With` metadata)) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillCombat 2)
              , ChooseFightEnemy iid source Nothing SkillCombat mempty False
              ]
    PassedSkillTest iid (Just Action.Fight) (isSource attrs -> True) SkillTestInitiatorTarget {} _ n
      | n >= 1 ->
          do
            push (skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1))
            if n >= 3 && not (gotExtraAction metadata)
              then do
                push (GainActions iid (toSource attrs) 1)
                pure $ FortyOneDerringer2 (attrs `With` Metadata True)
              else pure a
    EndTurn (controlledBy attrs -> True) ->
      pure $ FortyOneDerringer2 (attrs `With` Metadata False)
    _ -> FortyOneDerringer2 . (`with` metadata) <$> runMessage msg attrs
