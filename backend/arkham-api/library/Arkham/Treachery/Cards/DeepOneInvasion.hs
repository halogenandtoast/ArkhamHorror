module Arkham.Treachery.Cards.DeepOneInvasion (deepOneInvasion, DeepOneInvasion (..)) where

import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location
import Arkham.Helpers.Scenario
import Arkham.Keyword (Keyword (Hunter))
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeepOneInvasion = DeepOneInvasion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneInvasion :: TreacheryCard DeepOneInvasion
deepOneInvasion = treachery DeepOneInvasion Cards.deepOneInvasion

instance RunMessage DeepOneInvasion where
  runMessage msg t@(DeepOneInvasion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      shuffleEncounterDiscardBackIn

      grid <- getGrid
      mpos <- runMaybeT do
        loc <- MaybeT $ getLocationOf iid
        hoistMaybe $ findInGrid loc grid

      for_ mpos \(Pos x y) ->
        for_ [0 .. (abs x - 1)] \x' ->
          for_ (viewGrid (Pos (-x') y) grid) \(GridLocation _ loc) -> forTarget loc $ push msg
      pure t
    ForTarget (LocationTarget lid) (Revelation iid (isSource attrs -> True)) -> do
      discardUntilFirst
        iid
        (proxy lid attrs)
        Deck.EncounterDeck
        (basic $ CardWithTrait DeepOne <> CardWithKeyword Hunter)
      pure t
    RequestedEncounterCard (ProxySource (LocationSource lid) (isSource attrs -> True)) _ (Just card) -> do
      createEnemyAt_ card lid
      pure t
    _ -> DeepOneInvasion <$> liftRunMessage msg attrs
