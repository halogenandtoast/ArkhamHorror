module Arkham.Asset.Assets.JohnRaymondLegrasseHuntingForAnswers (johnRaymondLegrasse) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy, withSkillTest)
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (getCthulhuRage)
import Arkham.Trait (Trait (Cthulhu, DeepOne, StarSpawn))

newtype JohnRaymondLegrasseHuntingForAnswers = JohnRaymondLegrasseHuntingForAnswers AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

johnRaymondLegrasse :: AssetCard JohnRaymondLegrasseHuntingForAnswers
johnRaymondLegrasse = ally JohnRaymondLegrasseHuntingForAnswers Cards.johnRaymondLegrasse (3, 3)

-- "You get +1 skill value while fighting [[Deep One]] and [[Star Spawn]]
-- enemies." Applied to the controller only during a fight skill test whose
-- targeted enemy has the Deep One or Star Spawn trait.
instance HasModifiersFor JohnRaymondLegrasseHuntingForAnswers where
  getModifiersFor (JohnRaymondLegrasseHuntingForAnswers a) = for_ a.controller \iid -> do
    bonus <-
      fmap (fromMaybe []) $ runMaybeT do
        Action.Fight <- MaybeT getSkillTestAction
        enemy <- MaybeT getSkillTestTargetedEnemy
        matchesTrait <- lift $ enemy <=~> oneOf [EnemyWithTrait DeepOne, EnemyWithTrait StarSpawn]
        guard matchesTrait
        pure [AnySkillValue 1]
    modified_ a iid bonus

instance HasAbilities JohnRaymondLegrasseHuntingForAnswers where
  getAbilities (JohnRaymondLegrasseHuntingForAnswers a) =
    -- [reaction] During a skill test while fighting or evading Cthulhu,
    -- exhaust John Raymond Legrasse. Modeled as a fast ability available during
    -- such a skill test (the codebase represents "during a skill test"
    -- reactions this way).
    [ controlled
        a
        1
        ( DuringSkillTest
            $ oneOf
              [ WhileAttackingAnEnemy (EnemyWithTrait Cthulhu)
              , WhileEvadingAnEnemy (EnemyWithTrait Cthulhu)
              ]
        )
        $ FastAbility (exhaust a)
    ]

instance RunMessage JohnRaymondLegrasseHuntingForAnswers where
  runMessage msg a@(JohnRaymondLegrasseHuntingForAnswers attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "You get +X skill value for this test, where X is Cthulhu's Rage."
      rage <- getCthulhuRage
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue rage)
      pure a
    _ -> JohnRaymondLegrasseHuntingForAnswers <$> liftRunMessage msg attrs
