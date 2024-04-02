module Arkham.Asset.Cards.FortyOneDerringer2 (fortyOneDerringer2, FortyOneDerringer2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Metadata = Metadata {gotExtraAction :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FortyOneDerringer2 = FortyOneDerringer2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyOneDerringer2 :: AssetCard FortyOneDerringer2
fortyOneDerringer2 =
  asset (FortyOneDerringer2 . (`with` Metadata False)) Cards.fortyOneDerringer2

instance HasAbilities FortyOneDerringer2 where
  getAbilities (FortyOneDerringer2 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyOneDerringer2 where
  runMessage msg a@(FortyOneDerringer2 (attrs `With` metadata)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifier source iid (SkillModifier #combat 2), chooseFight]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 1 -> do
      push (skillTestModifier attrs iid (DamageDealt 1))
      if n >= 3 && not (gotExtraAction metadata)
        then do
          push $ GainActions iid (attrs.ability 1) 1
          pure $ FortyOneDerringer2 (attrs `With` Metadata True)
        else pure a
    EndTurn (controlledBy attrs -> True) ->
      pure $ FortyOneDerringer2 (attrs `With` Metadata False)
    _ -> FortyOneDerringer2 . (`with` metadata) <$> runMessage msg attrs
