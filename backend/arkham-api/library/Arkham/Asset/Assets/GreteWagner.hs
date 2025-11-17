module Arkham.Asset.Assets.GreteWagner (greteWagner) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype GreteWagner = GreteWagner AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greteWagner :: AssetCard GreteWagner
greteWagner = ally GreteWagner Cards.greteWagner (3, 2)

instance HasModifiersFor GreteWagner where
  getModifiersFor (GreteWagner a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities GreteWagner where
  getAbilities (GreteWagner a) =
    [ controlled a 1 (ClueOnLocation <> youExist (InvestigatorCanDiscoverCluesAt YourLocation))
        $ triggered
          (IfEnemyDefeated #after You ByAny AnyEnemy)
          (exhaust a <> DamageCost (a.ability 1) (toTarget a) 1)
    ]

instance RunMessage GreteWagner where
  runMessage msg a@(GreteWagner attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> GreteWagner <$> liftRunMessage msg attrs
