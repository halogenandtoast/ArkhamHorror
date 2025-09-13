module Arkham.Location.Cards.Montmartre209 (montmartre209) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Playable
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Window qualified as Window

newtype Montmartre209 = Montmartre209 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre209 :: LocationCard Montmartre209
montmartre209 = location Montmartre209 Cards.montmartre209 3 (PerPlayer 1)

instance HasAbilities Montmartre209 where
  getAbilities (Montmartre209 attrs) =
    extendRevealed1 attrs $ groupLimit PerRound $ restricted attrs 1 Here actionAbility

instance RunMessage Montmartre209 where
  runMessage msg a@(Montmartre209 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ TopOfDeckOf UneliminatedInvestigator
      iids <- select UneliminatedInvestigator
      playable <-
        filterPlayable iid (attrs.ability 1) (UnpaidCost NoAction) (Window.defaultWindows iid)
          $ filterCards (card_ #asset) cards

      temporaryModifiersMany attrs (map (,[TopCardOfDeckIsRevealed]) iids) do
        focusCards cards do
          chooseOneM iid do
            withI18n $ labeled' "playNoCards" nothing
            targets playable $ playCardPayingCost iid
      eachInvestigator shuffleDeck
      pure a
    _ -> Montmartre209 <$> liftRunMessage msg attrs
