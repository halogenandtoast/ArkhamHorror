module Arkham.Location.Cards.ChapelAtticSpectral_175 (chapelAtticSpectral_175) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Scenarios.TheWagesOfSin.Helpers

newtype ChapelAtticSpectral_175 = ChapelAtticSpectral_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelAtticSpectral_175 :: LocationCard ChapelAtticSpectral_175
chapelAtticSpectral_175 = location ChapelAtticSpectral_175 Cards.chapelAtticSpectral_175 4 (Static 0)

instance HasAbilities ChapelAtticSpectral_175 where
  getAbilities (ChapelAtticSpectral_175 a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ DrawCard #after You (basic NonWeakness) (DeckOf You)
      , mkAbility a 2 $ freeReaction $ SkillTestResult #after You (WhileInvestigating $ be a) #success
      , scenarioI18n
          $ withI18nTooltip "chapelAtticSpectral_175.haunted"
          $ restricted a 3 hauntedCriteria Haunted
      ]
   where
    hauntedCriteria = if null (locationCardsUnderneath a) then Never else NoRestriction

instance RunMessage ChapelAtticSpectral_175 where
  runMessage msg l@(ChapelAtticSpectral_175 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.chapelAttic_175
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      placeUnderneath attrs [card]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator \iid -> do
        let
          cards =
            filter (maybe False ((== Just iid) . pcOwner) . preview _PlayerCard) (locationCardsUnderneath attrs)
        unless (null cards) $ addToHand iid cards
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      for_ (nonEmpty $ locationCardsUnderneath attrs) $ sample >=> discardCard iid (attrs.ability 3)
      pure l
    _ -> ChapelAtticSpectral_175 <$> liftRunMessage msg attrs
