module Arkham.Homebrew.AgainstTheWendigo.Assets.GoldMiningRevenues (goldMiningRevenues) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype GoldMiningRevenues = GoldMiningRevenues AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goldMiningRevenues :: AssetCard GoldMiningRevenues
goldMiningRevenues = asset GoldMiningRevenues Cards.goldMiningRevenues

instance HasAbilities GoldMiningRevenues where
  getAbilities (GoldMiningRevenues a) =
    [ -- "You start each new scenario with 2 additional resources."
      restricted a 1 ControlsThis $ forced $ GameBegins #when
    , -- "Every new scenario, at the beginning of the first round: Test
      -- [willpower] (3). If you fail, take 1 direct horror."
      restricted a 2 ControlsThis $ forced $ RoundBegins #when
    ]

instance RunMessage GoldMiningRevenues where
  runMessage msg a@(GoldMiningRevenues attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      directHorror iid (attrs.ability 2) 1
      pure a
    _ -> GoldMiningRevenues <$> liftRunMessage msg attrs
