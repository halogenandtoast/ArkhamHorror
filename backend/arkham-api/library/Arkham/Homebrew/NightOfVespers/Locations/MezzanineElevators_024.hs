module Arkham.Homebrew.NightOfVespers.Locations.MezzanineElevators_024 (mezzanineElevators_024) where

import Arkham.Ability
import Arkham.Card
import Arkham.Discard
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardEdit)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype MezzanineElevators_024 = MezzanineElevators_024 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mezzanineElevators_024 :: LocationCard MezzanineElevators_024
mezzanineElevators_024 =
  symbolLabel $ location MezzanineElevators_024 Cards.mezzanineElevators_024 3 (PerPlayer 1)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities MezzanineElevators_024 where
  getAbilities (MezzanineElevators_024 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage MezzanineElevators_024 where
  runMessage msg l@(MezzanineElevators_024 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.luxurySuites_021
          , Cards.luxurySuites_022
          , Cards.presidentialSuite_027
          , Cards.presidentialSuite_028
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      randomDiscardEdit iid (attrs.ability 2) \d -> d {discardTarget = Just (toTarget attrs)}
      pure l
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      let source = attrs.ability 2
      for_ cards \card -> do
        let notThisCard = not_ $ CardWithId (toCardId card)
        if
          | card `cardMatch` card_ #asset -> do
              discards <- select $ inDiscardOf iid <> basic notThisCard
              chooseOneM iid $ withI18n do
                labeled "doNotAddToHand" nothing
                targets discards $ addToHand iid . only
          | card `cardMatch` card_ #event ->
              search iid source iid [fromTopOfDeck 3] (basic $ #event <> notThisCard) (AddFoundToHand iid 1)
          | card `cardMatch` card_ #skill ->
              gainResources iid source (length . cdSkills $ toCardDef card)
          | otherwise -> pure ()
      pure l
    _ -> MezzanineElevators_024 <$> liftRunMessage msg attrs
