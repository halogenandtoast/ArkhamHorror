module Arkham.Homebrew.AgesUnwound.Locations.ThePast (thePast) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Message (pattern R3)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorDiscard, InvestigatorHand))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype ThePast = ThePast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePast :: LocationCard ThePast
thePast = symbolLabel $ location ThePast Cards.thePast 4 (PerPlayer 1)

instance HasAbilities ThePast where
  getAbilities (ThePast a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnBegins #when You
      , groupLimit PerCampaign $ restricted a 2 Here doubleActionAbility
      ]

instance RunMessage ThePast where
  runMessage msg l@(ThePast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hand <- fieldMap InvestigatorHand (filterCards NonWeakness) iid
      for_ (nonEmpty hand) \cards -> do
        card <- sample cards
        shuffleCardsIntoDeck iid (only card)
      discarded <- fieldMap InvestigatorDiscard (filterCards NonWeakness) iid
      for_ (nonEmpty discarded) \cards -> do
        card <- sample cards
        addToHand iid [PlayerCard card]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure l
    _ -> ThePast <$> liftRunMessage msg attrs
