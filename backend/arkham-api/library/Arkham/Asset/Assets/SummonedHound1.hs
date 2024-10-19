module Arkham.Asset.Assets.SummonedHound1 (summonedHound1, SummonedHound1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Investigate
import Arkham.Investigate.Types qualified as Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Prelude
import Arkham.Projection

newtype SummonedHound1 = SummonedHound1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

summonedHound1 :: AssetCard SummonedHound1
summonedHound1 = assetWith SummonedHound1 Cards.summonedHound1 $ healthL ?~ 3

instance HasAbilities SummonedHound1 where
  getAbilities (SummonedHound1 attrs) =
    [ controlledAbility attrs 1 (Negate DuringAction <> DuringTurn You)
        $ FastAbility' (exhaust attrs) [#fight]
    , controlledAbility attrs 1 (Negate DuringAction <> DuringTurn You)
        $ FastAbility' (exhaust attrs) [#investigate]
    ]

instance RunMessage SummonedHound1 where
  runMessage msg a@(SummonedHound1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) _ -> do
      fightableEnemies <- select $ CanFightEnemy (toSource attrs)
      player <- getPlayer iid
      mLocation <- field InvestigatorLocation iid
      sid <- getRandom
      doFight <- toMessage <$> mkChooseFight sid iid (attrs.ability 1)
      mDoInvestigate :: Maybe Investigate.Investigate <- case mLocation of
        Just lid -> do
          canInvestigate <- lid <=~> InvestigatableLocation
          doInvestigate <- mkInvestigate sid iid (toAbilitySource attrs 1)
          pure $ guard canInvestigate $> doInvestigate
        Nothing -> pure Nothing

      case (fightableEnemies, mDoInvestigate) of
        ([], Nothing) -> error "invalid call"
        ([], Just doInvestigate) -> do
          enabled <- skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #intellect 5)
          pushAll [enabled, toMessage doInvestigate]
        (_ : _, Nothing) -> do
          enabled <- skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #combat 5)
          pushAll [enabled, doFight]
        (_ : _, Just doInvestigate) -> do
          intellectEnabled <- skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #intellect 5)
          combatEnabled <- skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #combat 5)
          push
            $ chooseOne
              player
              [ Label "Investigate" [intellectEnabled, toMessage doInvestigate]
              , Label "Fight" [combatEnabled, doFight]
              ]
      pure a
    _ -> SummonedHound1 <$> runMessage msg attrs
