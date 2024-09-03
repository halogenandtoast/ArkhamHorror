module Arkham.Treachery.Cards.CallOfTheUnknown (callOfTheUnknown, CallOfTheUnknown (..)) where

import Arkham.Ability
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History
import Arkham.Id
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Metadata = Metadata {chosenLocation :: Maybe LocationId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CallOfTheUnknown = CallOfTheUnknown (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfTheUnknown :: TreacheryCard CallOfTheUnknown
callOfTheUnknown = treachery (CallOfTheUnknown . (`with` Metadata Nothing)) Cards.callOfTheUnknown

instance HasAbilities CallOfTheUnknown where
  getAbilities (CallOfTheUnknown (a `With` _)) =
    [restrictedAbility a 1 (InThreatAreaOf You) $ forced (TurnBegins #when You)]

instance RunMessage CallOfTheUnknown where
  runMessage msg t@(CallOfTheUnknown (attrs `With` metadata)) = runQueueT $ case msg of
    Revelation iid source | isSource attrs source -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid source 1 windows' payment | isSource attrs source -> do
      targets <- selectTargets $ not_ $ locationWithInvestigator iid
      when (notNull targets)
        $ chooseOne
          iid
          [ targetLabel target [UseCardAbilityChoiceTarget iid source 1 target windows' payment]
          | target <- targets
          ]
      pure t
    UseCardAbilityChoiceTarget _ (isSource attrs -> True) 1 (LocationTarget lid) _ _ -> do
      pure $ CallOfTheUnknown $ attrs `with` Metadata (Just lid)
    When (EndTurn iid) | treacheryInThreatArea iid attrs -> do
      -- use When here to trigger before turn history is erased
      for_ (chosenLocation metadata) \lid -> do
        history <- getHistory TurnHistory iid
        when (lid `notMember` historyLocationsSuccessfullyInvestigated history) do
          assignHorror iid attrs 2
          shuffleIntoDeck iid attrs
      pure $ CallOfTheUnknown $ attrs `with` Metadata Nothing
    _ -> CallOfTheUnknown . (`with` metadata) <$> liftRunMessage msg attrs
