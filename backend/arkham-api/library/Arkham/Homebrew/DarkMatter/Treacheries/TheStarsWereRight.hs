module Arkham.Homebrew.DarkMatter.Treacheries.TheStarsWereRight (theStarsWereRight) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype TheStarsWereRight = TheStarsWereRight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStarsWereRight :: TreacheryCard TheStarsWereRight
theStarsWereRight = treachery TheStarsWereRight Cards.theStarsWereRight

{- | "Revelation - Attach this card to the location with the most clues without a
copy of The Stars Were Right attached.
Forced - After you fail a skill test by 3 or more at the attached location: Place
1 doom on The Stars Were Right.
[action]: Test [willpower] (3). If you succeed, discard this card."
-}
instance HasAbilities TheStarsWereRight where
  getAbilities (TheStarsWereRight a) =
    [ restricted a 1 (youExist $ at_ (LocationWithTreachery $ TreacheryWithId a.id))
        $ forced
        $ SkillTestResult #after You AnySkillTest (FailureResult $ atLeast 3)
    , skillTestAbility $ restricted a 2 (youExist $ at_ (LocationWithTreachery $ TreacheryWithId a.id)) actionAbility
    ]

instance RunMessage TheStarsWereRight where
  runMessage msg t@(TheStarsWereRight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      candidates <-
        select
          $ LocationWithMostClues
          $ not_ (LocationWithTreachery $ treacheryIs Cards.theStarsWereRight)
      if null candidates
        then toDiscard attrs attrs
        else chooseOrRunOneM iid $ targets candidates $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> TheStarsWereRight <$> liftRunMessage msg attrs
