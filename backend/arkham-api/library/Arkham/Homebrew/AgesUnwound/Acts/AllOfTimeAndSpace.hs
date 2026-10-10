module Arkham.Homebrew.AgesUnwound.Acts.AllOfTimeAndSpace (allOfTimeAndSpace) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Agenda (getDoomOnAgenda)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype AllOfTimeAndSpace = AllOfTimeAndSpace ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 1. The printed clue requirement is the act's own advancement cost, so
'Arkham.Act.Types' publishes the Objective; ability 1 is the printed move.
-}
allOfTimeAndSpace :: ActCard AllOfTimeAndSpace
allOfTimeAndSpace =
  act (1, A) AllOfTimeAndSpace Cards.allOfTimeAndSpace
    $ Just (GroupClueCost (PerPlayer 5) Anywhere)

{- | "[action]: __Move.__ Test [willpower] (5). This test gets -1 difficulty for
each doom on the current agenda. If you succeed, move to any [[Adrift]]
location. If you fail, move to a random other [[Adrift]] location."

A printed __Move__, so the ability carries the move action: it costs an action,
provokes attacks of opportunity and counts as a move for everything that cares.
-}
instance HasAbilities AllOfTimeAndSpace where
  getAbilities (AllOfTimeAndSpace a) =
    extend a [mkAbility a 1 $ ActionAbility #move Nothing (ActionCost 1)]

instance RunMessage AllOfTimeAndSpace where
  runMessage msg a@(AllOfTimeAndSpace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      doom <- getDoomOnAgenda
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed $ max 0 (5 - doom))
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      destinations <- select $ LocationWithTrait Adrift
      chooseTargetM iid destinations $ moveTo (attrs.ability 1) iid
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveToRandomOtherAdrift (attrs.ability 1) iid
      pure a
    {- "A Path Appears. Put the set-aside The Timestream and Arkham,
    Massachusetts (Present Day?) locations into play." Both come out of the
    set-aside pool, which is why they are placed rather than minted -- a fresh
    card would never match the pile's copy. The Timestream takes the layout's
    centre cell; Arkham sits beside it. -}
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      timestream <- placeSetAsideLocation Locations.theTimestream
      push $ SetLocationLabel timestream "timestream"
      arkham <- placeSetAsideLocation Locations.arkhamMassachusetts_086
      push $ SetLocationLabel arkham "arkham"
      advanceActDeck attrs
      pure a
    _ -> AllOfTimeAndSpace <$> liftRunMessage msg attrs
