module Arkham.Homebrew.AgesUnwound.Acts.AllOfTimeAndSpace (allOfTimeAndSpace) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
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

allOfTimeAndSpace :: ActCard AllOfTimeAndSpace
allOfTimeAndSpace =
  act (1, A) AllOfTimeAndSpace Cards.allOfTimeAndSpace
    $ Just (GroupClueCost (PerPlayer 5) Anywhere)

instance HasAbilities AllOfTimeAndSpace where
  getAbilities (AllOfTimeAndSpace a) =
    extend a [mkAbility a 1 $ ActionAbility #move Nothing (ActionCost 1)]

instance RunMessage AllOfTimeAndSpace where
  runMessage msg a@(AllOfTimeAndSpace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower
        $ MinCalculation (Fixed 0) (SubtractCalculation (Fixed 5) CurrentAgendaDoomCountCalculation)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      destinations <- select $ LocationWithTrait Adrift
      chooseTargetM iid destinations $ moveTo (attrs.ability 1) iid
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveToRandomOtherAdrift (attrs.ability 1) iid
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      timestream <- placeSetAsideLocation Locations.theTimestream
      setLocationLabel timestream "timestream"
      arkham <- placeSetAsideLocation Locations.arkhamMassachusetts_086
      setLocationLabel arkham "arkham"
      advanceActDeck attrs
      pure a
    _ -> AllOfTimeAndSpace <$> liftRunMessage msg attrs
