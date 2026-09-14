module Arkham.Homebrew.DarkMatter.Assets.StasisCube (stasisCube) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (Semaphore), semaphore)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffImpendingDoom)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StasisCube = StasisCube AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stasisCube :: AssetCard StasisCube
stasisCube = asset StasisCube Cards.stasisCube

{- | "[action] [action] Test [intellect] (3): If you succeed, remove 1 doom from
the current agenda and cross out 1 tally mark under Impending Doom in your
Campaign Log. (Group limit one success per game.)"

The limit is one /success/, not one use, so it is a semaphore on the effect
rather than a limit on the ability.
-}
instance HasAbilities StasisCube where
  getAbilities (StasisCube a) =
    [skillTestAbility $ controlled_ a 1 $ doubleActionAbilityWithCost mempty]

instance RunMessage StasisCube where
  runMessage msg a@(StasisCube attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      chooseTargetM iid investigators \bearer -> putCardIntoPlay bearer attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        gameModifier (attrs.ability 1) attrs Semaphore
        selectOne AnyAgenda >>= traverse_ \agenda -> removeDoom (attrs.ability 1) agenda 1
        crossOffImpendingDoom 1
      pure a
    _ -> StasisCube <$> liftRunMessage msg attrs
