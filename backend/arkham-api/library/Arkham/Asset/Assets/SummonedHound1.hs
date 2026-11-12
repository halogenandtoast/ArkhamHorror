module Arkham.Asset.Assets.SummonedHound1 (summonedHound1) where

import Arkham.Ability
import Arkham.Actions (orActions)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.CombatTarget
import Arkham.Helpers.Cost (getCanAffordAdditionalActionCost)
import Arkham.Helpers.Investigator (getJustLocation, getMaybeLocation)
import Arkham.Investigate (mkInvestigateLocation)
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
    [ delayAdditionalCosts
        $ controlled attrs 1 (not_ DuringAction <> DuringTurn You)
        $ FastAbility' (exhaust attrs) (orActions [#fight, #investigate])
    ]

{- The ability is one printed Fight/Investigate, so its designated actions are an
'OrActions' and activation cannot know which branch is coming. 'delayAdditionalCosts'
holds back every additional cost until it does -- including the action surcharge, or
Frozen in Fear's first-fight charge landed on an investigate (#5990) -- so each branch
pays for itself through 'CheckAdditionalActionCosts'. -}
instance RunMessage SummonedHound1 where
  runMessage msg a@(SummonedHound1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) _ -> do
      sid <- getRandom
      mlid <- getMaybeLocation iid
      -- Each branch is only offered if its own deferred cost can be paid: activation no
      -- longer vouches for either, so an unaffordable one would be a dead-end choice.
      canFight <-
        andM
          [ hasFightTargets (toSource attrs) iid
          , getCanAffordAdditionalActionCost iid (attrs.ability 1) (toTarget iid) #fight
          ]
      canInvestigate <- case mlid of
        Nothing -> pure False
        Just lid ->
          andM
            [ lid <=~> InvestigatableLocation
            , getCanAffordAdditionalActionCost iid (attrs.ability 1) (toTarget lid) #investigate
            ]
      chooseOrRunOneM iid do
        when canFight do
          labeledI "fight" do
            skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #combat 5)
            fight <- mkChooseFight sid iid (attrs.ability 1)
            push
              $ CheckAdditionalActionCosts
                iid
                (toTarget iid)
                #fight
                [toMessage fight {chooseFightIsAction = True, chooseFightPayCost = False}]
        when canInvestigate do
          labeledI "investigate" do
            lid <- getJustLocation iid
            skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #intellect 5)
            investigate' <- mkInvestigateLocation sid iid (attrs.ability 1) lid
            push
              $ CheckAdditionalActionCosts
                iid
                (toTarget lid)
                #investigate
                [toMessage investigate' {investigateIsAction = True, investigatePayCost = False}]
      pure a
    _ -> SummonedHound1 <$> liftRunMessage msg attrs
