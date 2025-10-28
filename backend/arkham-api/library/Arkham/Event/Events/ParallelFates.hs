module Arkham.Event.Events.ParallelFates (parallelFates) where

import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Strategy
import Arkham.Zone

newtype ParallelFates = ParallelFates EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parallelFates :: EventCard ParallelFates
parallelFates = event ParallelFates Cards.parallelFates

instance RunMessage ParallelFates where
  runMessage msg e@(ParallelFates attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lookAt iid attrs EncounterDeckTarget [fromTopOfDeck 4] #any (defer attrs IsNotDraw)
      pure e
    SearchFound _ (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards $ requestChaosTokens attrs.owner attrs 1
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map chaosTokenFace -> tokens) -> do
      if any (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) tokens
        then chooseOneM iid $ labeled "Shuffle back in" nothing -- shuffle is default
        else
          chooseOneM iid
            $ labeled "Put Back on Top in any order"
            $ push
            $ UpdateSearchReturnStrategy iid FromDeck PutBackInAnyOrder
      pure e
    _ -> ParallelFates <$> liftRunMessage msg attrs
