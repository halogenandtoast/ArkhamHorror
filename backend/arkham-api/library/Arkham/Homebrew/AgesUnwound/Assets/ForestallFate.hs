module Arkham.Homebrew.AgesUnwound.Assets.ForestallFate (forestallFate) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (getCanLoseActions)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ForestallFate = ForestallFate AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Uses (4 charges)" and the printed __Fast__ both ride on the def.
forestallFate :: AssetCard ForestallFate
forestallFate = asset ForestallFate Cards.forestallFate

{- | "[free] Exhaust Forestall Fate and spend 1 charge: Test [willpower] (3). If
you succeed, gain an action. If a [elder_thing] or [auto_fail] token is revealed
during this skill test, take 2 horror or lose an action."
-}
instance HasAbilities ForestallFate where
  getAbilities (ForestallFate a) =
    [ skillTestAbility
        $ controlled_ a 1
        $ FastAbility (exhaust a <> assetUseCost a Charge 1)
    ]

{- | The token rider is independent of the result, so it is an
'onRevealChaosTokenEffect' that defers to 'afterThisTestResolves' -- Rite of
Seeking's shape, which is the same sentence.

"Lose N actions" is 'loseActions', not 'loseUnrestrictedActions': the campaign's
/standard action/ term governs the effects that *count* actions, while losing
follows the core rule -- remaining actions go first, and an investigator left
with only additional actions loses those, whatever their limitation (FAQ 1.10).
With nothing at all to give up, the horror is the only option left.
-}
instance RunMessage ForestallFate where
  runMessage msg a@(ForestallFate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#elderthing, #autofail]) (attrs.ability 1) attrs do
        -- `doStep` and not the choice itself: `afterThisTestResolves` captures its
        -- body when the token is revealed, so the "can I lose an action" read has
        -- to happen in a handler that runs once the test is over.
        afterThisTestResolves sid $ doStep 1 msg
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      canLose <- getCanLoseActions iid
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 2
        countVar 1 $ labeledValidate canLose "loseActions" $ loseActions iid (attrs.ability 1) 1
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainActions iid (attrs.ability 1) 1
      pure a
    _ -> ForestallFate <$> liftRunMessage msg attrs
