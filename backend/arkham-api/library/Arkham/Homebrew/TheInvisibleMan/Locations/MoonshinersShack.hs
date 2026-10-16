{- | Moonshiners' Shack -- the men in the woods know who has been stealing.

"(Group limit 1 success per game.)" limits the /success/ and not the attempt, so
it is a 'Semaphore' on the effect in the manner of the Stasis Cube rather than a
limit on the ability: a failed interrogation may be tried again.

The printed "+1 difficulty" is a separate clause on a base-2 test, so it is a
modifier on the test rather than a difficulty of 3.
-}
module Arkham.Homebrew.TheInvisibleMan.Locations.MoonshinersShack (moonshinersShack) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_, semaphore)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype MoonshinersShack = MoonshinersShack LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonshinersShack :: LocationCard MoonshinersShack
moonshinersShack = location MoonshinersShack Cards.moonshinersShack 4 (PerPlayer 1)

-- | "This test gets +1 difficulty."
instance HasModifiersFor MoonshinersShack where
  getModifiersFor (MoonshinersShack a) = whenRevealed a $ whenJustM getSkillTest \st ->
    maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      pure [Difficulty 1]

instance HasAbilities MoonshinersShack where
  getAbilities (MoonshinersShack a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "moonshinersShack.interrogate"
      $ skillTestAbility
      $ restricted a 1 Here #parley

instance RunMessage MoonshinersShack where
  runMessage msg l@(MoonshinersShack attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #combat] \kind ->
          skillLabeled kind $ parley sid iid (attrs.ability 1) attrs kind (Fixed 2)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        gameModifier (attrs.ability 1) attrs Semaphore
        placeLead (attrs.ability 1) attrs
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> MoonshinersShack <$> liftRunMessage msg attrs
