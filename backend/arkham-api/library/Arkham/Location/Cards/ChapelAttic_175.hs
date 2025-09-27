module Arkham.Location.Cards.ChapelAttic_175 (chapelAttic_175) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ChapelAttic_175 = ChapelAttic_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelAttic_175 :: LocationCard ChapelAttic_175
chapelAttic_175 = location ChapelAttic_175 Cards.chapelAttic_175 4 (Static 0)

instance HasAbilities ChapelAttic_175 where
  getAbilities (ChapelAttic_175 a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ DrawCard #after You (basic NonWeakness) (DeckOf You)
      , mkAbility a 2 $ freeReaction $ SkillTestResult #after You (WhileInvestigating $ be a) #success
      ]

toDrawn :: [Window] -> Card
toDrawn [] = error "invalid call"
toDrawn ((windowType -> Window.DrawCard _ card _) : _) = card
toDrawn (_ : xs) = toDrawn xs

instance RunMessage ChapelAttic_175 where
  runMessage msg l@(ChapelAttic_175 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.chapelAtticSpectral_175
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 (toDrawn -> card) _ -> do
      placeUnderneath attrs [card]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator \iid -> do
        let
          cards =
            filter (maybe False ((== Just iid) . pcOwner) . preview _PlayerCard) (locationCardsUnderneath attrs)
        unless (notNull cards) $ addToHand iid cards

      pure l
    _ -> ChapelAttic_175 <$> liftRunMessage msg attrs
