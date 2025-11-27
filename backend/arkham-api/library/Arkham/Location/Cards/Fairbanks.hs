module Arkham.Location.Cards.Fairbanks (fairbanks) where

import Arkham.Ability
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Hazard))

newtype Fairbanks = Fairbanks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fairbanks :: LocationCard Fairbanks
fairbanks = location Fairbanks Cards.fairbanks 2 (Static 0)

instance HasAbilities Fairbanks where
  getAbilities (Fairbanks a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist (InvestigatorWithSpendableResources $ atLeast 2))
      $ freeReaction
      $ oneOf
        [ DrawCard
            #when
            (affectsOthers $ investigatorAt a)
            (CanCancelRevelationEffect You $ basic $ NonPeril <> #treachery <> CardWithTrait Hazard)
            AnyDeck
        , DrawCard
            #when
            You
            (CanCancelRevelationEffect You $ basic $ #treachery <> CardWithTrait Hazard)
            AnyDeck
        ]

instance RunMessage Fairbanks where
  runMessage msg l@(Fairbanks attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> treachery) _ -> do
      spendResources iid 2
      cancelCardEffects attrs treachery
      pure l
    _ -> Fairbanks <$> liftRunMessage msg attrs
