module Arkham.Asset.Cards.SummonedHound1 (
  summonedHound1,
  SummonedHound1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate
import Arkham.Investigate.Types qualified as Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype SummonedHound1 = SummonedHound1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

summonedHound1 :: AssetCard SummonedHound1
summonedHound1 = assetWith SummonedHound1 Cards.summonedHound1 $ healthL ?~ 3

instance HasAbilities SummonedHound1 where
  getAbilities (SummonedHound1 attrs) =
    [ controlledAbility attrs 1 (Negate DuringAction) $ FastAbility' (exhaust attrs) [#fight]
    , controlledAbility attrs 1 (Negate DuringAction) $ FastAbility' (exhaust attrs) [#investigate]
    ]

instance RunMessage SummonedHound1 where
  runMessage msg a@(SummonedHound1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) _ -> do
      fightableEnemies <- select $ CanFightEnemy (toSource attrs)
      player <- getPlayer iid
      mLocation <- field InvestigatorLocation iid
      let doFight = chooseFightEnemy iid attrs #combat
      mDoInvestigate :: Maybe Investigate.Investigate <- case mLocation of
        Just lid -> do
          canInvestigate <- lid <=~> InvestigatableLocation
          doInvestigate <- mkInvestigate iid (toAbilitySource attrs 1)
          pure $ guard canInvestigate $> doInvestigate
        Nothing -> pure Nothing

      case (fightableEnemies, mDoInvestigate) of
        ([], Nothing) -> error "invalid call"
        ([], Just doInvestigate) ->
          pushAll
            [ skillTestModifier (toAbilitySource attrs 1) iid (BaseSkillOf #intellect 5)
            , toMessage doInvestigate
            ]
        (_ : _, Nothing) -> pushAll [skillTestModifier (toAbilitySource attrs 1) iid (BaseSkillOf #combat 5), doFight]
        (_ : _, Just doInvestigate) -> do
          push
            $ chooseOne
              player
              [ Label
                  "Investigate"
                  [ skillTestModifier (toAbilitySource attrs 1) iid (BaseSkillOf #intellect 5)
                  , toMessage doInvestigate
                  ]
              , Label "Fight" [skillTestModifier (toAbilitySource attrs 1) iid (BaseSkillOf #combat 5), doFight]
              ]
      pure a
    _ -> SummonedHound1 <$> runMessage msg attrs
