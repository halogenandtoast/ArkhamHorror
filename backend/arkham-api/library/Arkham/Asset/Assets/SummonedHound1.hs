module Arkham.Asset.Assets.SummonedHound1 (summonedHound1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Investigate.Types
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype SummonedHound1 = SummonedHound1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

summonedHound1 :: AssetCard SummonedHound1
summonedHound1 = assetWith SummonedHound1 Cards.summonedHound1 $ healthL ?~ 3

instance HasAbilities SummonedHound1 where
  getAbilities (SummonedHound1 attrs) =
    [ controlled attrs 1 (not_ DuringAction <> DuringTurn You)
        $ FastAbility' (exhaust attrs) [#fight]
    , controlled attrs 1 (not_ DuringAction <> DuringTurn You)
        $ FastAbility' (exhaust attrs) [#investigate]
    ]

instance RunMessage SummonedHound1 where
  runMessage msg a@(SummonedHound1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) _ -> do
      sid <- getRandom
      fightableEnemies <- select $ CanFightEnemy (toSource attrs)
      canInvestigate <- maybe (pure False) (`matches` InvestigatableLocation) =<< getMaybeLocation iid
      chooseOrRunOneM iid do
        unless (null fightableEnemies) do
          labeled "Fight" do
            skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #combat 5)
            chooseFightEnemyEdit sid iid (attrs.ability 1) \cf -> cf {chooseFightIsAction = True}
        when canInvestigate do
          labeled "Investigate" do
            skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #intellect 5)
            investigateEdit_ sid iid (attrs.ability 1) \i -> i {investigateIsAction = True}
      pure a
    _ -> SummonedHound1 <$> liftRunMessage msg attrs
