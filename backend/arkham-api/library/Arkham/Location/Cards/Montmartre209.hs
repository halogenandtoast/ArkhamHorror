module Arkham.Location.Cards.Montmartre209 (montmartre209, Montmartre209 (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (mkWhen)
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
      temporaryModifiers iid attrs [TopCardOfDeckIsRevealed, CanPlayTopOfDeck AnyCard] do
        cards <-
          filterM (getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) [mkWhen (Window.DuringTurn iid)])
            =<< select (TopOfDeckOf UneliminatedInvestigator)
        chooseOneM iid do
          labeled "Play no cards" nothing
          targets cards $ playCardPayingCost iid
      pure a
    _ -> Montmartre209 <$> liftRunMessage msg attrs
