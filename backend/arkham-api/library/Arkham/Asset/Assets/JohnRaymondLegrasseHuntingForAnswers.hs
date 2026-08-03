module Arkham.Asset.Assets.JohnRaymondLegrasseHuntingForAnswers (johnRaymondLegrasse) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy, withSkillTest)
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (getCthulhuRage)
import Arkham.Trait (Trait (DeepOne, StarSpawn))

newtype JohnRaymondLegrasseHuntingForAnswers = JohnRaymondLegrasseHuntingForAnswers AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

johnRaymondLegrasse :: AssetCard JohnRaymondLegrasseHuntingForAnswers
johnRaymondLegrasse = ally JohnRaymondLegrasseHuntingForAnswers Cards.johnRaymondLegrasse (3, 3)

{- | Cthulhu wears four faces in Part II, three of which carry the trait and one of
which (Ancient Evil, the board itself) does not, so match him by name.
-}
cthulhu :: EnemyMatcher
cthulhu = EnemyWithTitle "Cthulhu"

instance HasModifiersFor JohnRaymondLegrasseHuntingForAnswers where
  getModifiersFor (JohnRaymondLegrasseHuntingForAnswers a) = do
    -- "He does not take up an ally slot during Part I or Part II of this scenario",
    -- and the inspector only ever joins the investigators for those two.
    modifySelf a [DoNotTakeUpSlot #ally]
    -- "You get +1 skill value while fighting [[Deep One]] and [[Star Spawn]]
    -- enemies."
    for_ a.controller \iid -> do
      bonus <- fromMaybe [] <$> runMaybeT do
        Action.Fight <- MaybeT getSkillTestAction
        enemy <- MaybeT getSkillTestTargetedEnemy
        guardM $ lift $ enemy <=~> mapOneOf EnemyWithTrait [DeepOne, StarSpawn]
        pure [AnySkillValue 1]
      modified_ a iid bonus

instance HasAbilities JohnRaymondLegrasseHuntingForAnswers where
  getAbilities (JohnRaymondLegrasseHuntingForAnswers a) =
    -- "[reaction] During a skill test while fighting or evading Cthulhu, exhaust
    -- John Raymond Legrasse", which the engine models as a fast ability that is
    -- only available for the duration of such a test.
    [ controlled
        a
        1
        (DuringSkillTest $ oneOf [WhileAttackingAnEnemy cthulhu, WhileEvadingAnEnemy cthulhu])
        $ FastAbility (exhaust a)
    ]

instance RunMessage JohnRaymondLegrasseHuntingForAnswers where
  runMessage msg a@(JohnRaymondLegrasseHuntingForAnswers attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "You get +X skill value for this test, where X is Cthulhu's Rage."
      rage <- getCthulhuRage
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue rage)
      pure a
    _ -> JohnRaymondLegrasseHuntingForAnswers <$> liftRunMessage msg attrs
