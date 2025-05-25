module Arkham.Asset.Assets.DarioElAmin (darioElAmin) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype DarioElAmin = DarioElAmin AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darioElAmin :: AssetCard DarioElAmin
darioElAmin = ally DarioElAmin Cards.darioElAmin (2, 2)

instance HasModifiersFor DarioElAmin where
  getModifiersFor (DarioElAmin a) = for_ a.controller \iid -> do
    resources <- field InvestigatorResources iid
    modifiedWhen_ a (resources >= 10) iid [SkillModifier #willpower 1, SkillModifier #intellect 1]

instance HasAbilities DarioElAmin where
  getAbilities (DarioElAmin attrs) =
    [ controlled attrs 1 (exists $ YourLocation <> LocationWithoutEnemies)
        $ actionAbilityWithCost
        $ exhaust attrs
    ]

instance RunMessage DarioElAmin where
  runMessage msg a@(DarioElAmin attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      pure a
    _ -> DarioElAmin <$> liftRunMessage msg attrs
