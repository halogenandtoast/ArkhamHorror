module Arkham.Homebrew.AgesUnwound.Assets.ForestallFate (forestallFate) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ForestallFate = ForestallFate AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Uses (4 charges)."

TODO(ages-unwound): two things belong on the def and @CardDefs/Assets.hs@ is the
orchestrator's -- @cdUses = Uses Charge (Static 4)@ (set on the attrs here
instead, which is equivalent at runtime) and @cdFastWindow@ for the printed
__Fast__, which has no attrs equivalent, so the card still plays as an action.
-}
forestallFate :: AssetCard ForestallFate
forestallFate = assetWith ForestallFate Cards.forestallFate (printedUsesL .~ Uses Charge (Fixed 4))

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
-}
instance RunMessage ForestallFate where
  runMessage msg a@(ForestallFate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#elderthing, #autofail]) (attrs.ability 1) attrs do
        afterThisTestResolves sid $ chooseOneM iid $ withI18n do
          countVar 2 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 2
          countVar 1 $ labeled "loseActions" $ loseStandardActions iid (attrs.ability 1) 1
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainActions iid (attrs.ability 1) 1
      pure a
    _ -> ForestallFate <$> liftRunMessage msg attrs
