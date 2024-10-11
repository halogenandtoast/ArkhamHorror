module Arkham.Asset.Assets.SongOfTheDead2 (songOfTheDead2, SongOfTheDead2 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Fight
import Arkham.Modifier

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: AssetCard SongOfTheDead2
songOfTheDead2 = asset SongOfTheDead2 Cards.songOfTheDead2

instance HasAbilities SongOfTheDead2 where
  getAbilities (SongOfTheDead2 x) =
    [restricted x 1 ControlsThis $ fightAction (assetUseCost x Charge 1)]

instance RunMessage SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #willpower 1)
      onRevealChaosTokenEffect sid #skull source iid do
        skillTestModifier sid source iid (DamageDealt 2)
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> SongOfTheDead2 <$> liftRunMessage msg attrs
