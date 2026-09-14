module Arkham.Homebrew.DarkMatter.Enemies.TheGreys (theGreys) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest.Lifted (combinationSkillTestEdit)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.SkillTest.Base (setIsParley)
import Arkham.Window qualified as Window

newtype TheGreys = TheGreys EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreys :: EnemyCard TheGreys
theGreys = enemy TheGreys Cards.theGreys

{- | "[action]: Parley. Test [willpower] + [intellect] (3). Reveal and resolve an
additional chaos token for this skill test. If you succeed, place 1 clue on the
current act (from the token bank)."
-}
instance HasAbilities TheGreys where
  getAbilities (TheGreys a) =
    extend
      a
      [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
      , -- The Greys is never actually defeated (act 1b flips it instead); intercept
        -- the moment it would be so act 1's advance trigger still fires the window.
        mkAbility a 2 $ SilentForcedAbility $ EnemyWouldBeDefeated #when (be a)
      ]

instance RunMessage TheGreys where
  runMessage msg e@(TheGreys attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      cancelEnemyDefeat attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) sid RevealAnotherChaosToken
      combinationSkillTestEdit
        sid
        iid
        (attrs.ability 1)
        attrs
        [#willpower, #intellect]
        (Fixed 3)
        setIsParley
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      act <- selectJust AnyAct
      placeClues (attrs.ability 1) act 1
      pure e
    {- Act 1b: "Flip The Greys to its other side (transfer any damage tokens on it
    to its other side)." The other side is the Mi-Go Scientist; 'Swap' carries the
    tokens, placement and engagement over. -}
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceEnemy attrs.id (lookupCard Cards.miGoScientist attrs.cardId) Swap
      checkAfter $ Window.EnemyFlipped attrs.id
      pure e
    _ -> TheGreys <$> liftRunMessage msg attrs
