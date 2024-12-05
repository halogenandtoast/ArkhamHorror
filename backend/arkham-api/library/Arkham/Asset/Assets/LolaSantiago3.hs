module Arkham.Asset.Assets.LolaSantiago3 (lolaSantiago3, LolaSantiago3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Cost.FieldCost
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher

newtype LolaSantiago3 = LolaSantiago3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lolaSantiago3 :: AssetCard LolaSantiago3
lolaSantiago3 = ally LolaSantiago3 Cards.lolaSantiago3 (2, 2)

instance HasModifiersFor LolaSantiago3 where
  getModifiersFor (LolaSantiago3 a) = controllerGets a [SkillModifier #intellect 1, SkillModifier #agility 1]

instance HasAbilities LolaSantiago3 where
  getAbilities (LolaSantiago3 a) =
    [ controlledAbility a 1 ClueOnLocation
        $ FastAbility (exhaust a <> MaybeFieldResourceCost (MaybeFieldCost YourLocation LocationShroud))
    ]

instance RunMessage LolaSantiago3 where
  runMessage msg a@(LolaSantiago3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> LolaSantiago3 <$> liftRunMessage msg attrs
