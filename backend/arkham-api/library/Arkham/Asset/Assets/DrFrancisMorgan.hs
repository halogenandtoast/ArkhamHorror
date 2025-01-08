module Arkham.Asset.Assets.DrFrancisMorgan (drFrancisMorgan, DrFrancisMorgan (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype DrFrancisMorgan = DrFrancisMorgan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drFrancisMorgan :: AssetCard DrFrancisMorgan
drFrancisMorgan = ally DrFrancisMorgan Cards.drFrancisMorgan (4, 1)

instance HasAbilities DrFrancisMorgan where
  getAbilities (DrFrancisMorgan x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility (Matcher.EnemyDefeated #after You ByAny AnyEnemy) (exhaust x)
    ]

instance HasModifiersFor DrFrancisMorgan where
  getModifiersFor (DrFrancisMorgan a) = controllerGets a [SkillModifier #combat 1]

instance RunMessage DrFrancisMorgan where
  runMessage msg a@(DrFrancisMorgan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> DrFrancisMorgan <$> liftRunMessage msg attrs
