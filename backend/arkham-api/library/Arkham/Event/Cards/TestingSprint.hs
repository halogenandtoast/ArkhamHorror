module Arkham.Event.Cards.TestingSprint (testingSprint, TestingSprint (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Id
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Zone
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map

newtype Meta = Meta {remainingLocations :: [(Int, NonEmpty LocationId)]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TestingSprint = TestingSprint (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

testingSprint :: EventCard TestingSprint
testingSprint = event (TestingSprint . (`with` Meta [])) Cards.testingSprint

instance RunMessage TestingSprint where
  runMessage msg e@(TestingSprint (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <-
        sortOn fst
          . Map.toList
          . Map.unionsWith (<>)
          . mapMaybe (\(x, my) -> Map.singleton <$> my <*> pure (NE.singleton x))
          <$> selectWithField
            LocationShroud
            ( InvestigatableLocation
                <> oneOf [locationWithInvestigator iid, ConnectedFrom (locationWithInvestigator iid)]
            )
      push $ Do msg
      pure . TestingSprint $ attrs `with` Meta locations
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      case remainingLocations meta of
        [] -> pure e
        ((_, x :| []) : ys) -> do
          push $ handleTargetChoice iid attrs x
          pure . TestingSprint $ attrs `with` Meta ys
        ((_, x :| xs) : _) -> do
          chooseOneToHandle iid attrs (x : xs)
          pure e
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      let filterChoice (n, (x :| xs)) = (n,) <$> nonEmpty (deleteFirst lid (x : xs))
      let meta' = mapMaybe filterChoice (remainingLocations meta)
      sid <- getRandom
      pushM $ mkInvestigateLocation sid iid attrs lid
      pure . TestingSprint $ attrs `with` Meta meta'
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      afterSkillTest do
        push $ Do (InvestigatorPlayEvent iid attrs.id Nothing [] FromHand)
      pure e
    _ -> TestingSprint . (`with` meta) <$> liftRunMessage msg attrs
