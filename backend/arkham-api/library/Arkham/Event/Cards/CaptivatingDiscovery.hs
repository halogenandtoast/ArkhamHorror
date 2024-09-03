module Arkham.Event.Cards.CaptivatingDiscovery (captivatingDiscovery, CaptivatingDiscovery (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Search
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Strategy

newtype CaptivatingDiscovery = CaptivatingDiscovery EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captivatingDiscovery :: EventCard CaptivatingDiscovery
captivatingDiscovery = event CaptivatingDiscovery Cards.captivatingDiscovery

instance RunMessage CaptivatingDiscovery where
  runMessage msg e@(CaptivatingDiscovery attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search iid attrs iid [fromTopOfDeck 6] #any (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      n <- min 3 <$> field InvestigatorClues iid
      focusCards cards \unfocus -> do
        when (n > 0)
          $ chooseAmount iid "Clues" "Clues" 0 n attrs
        push unfocus
      pure e
    ResolveAmounts iid (getChoiceAmount "Clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      pushAll [InvestigatorPlaceCluesOnLocation iid (attrs.ability 1) n, DoStep n msg]
      pure e
    DoStep n msg'@(ResolveAmounts iid _ (isTarget attrs -> True)) | n > 0 -> do
      cards <- getFoundCards iid
      focusCards cards \unfocus -> do
        chooseN iid (min 2 $ length cards) [targetLabel card [AddToHand iid [card]] | card <- cards]
        push unfocus
      push $ DoStep (n - 1) msg'
      pure e
    _ -> CaptivatingDiscovery <$> liftRunMessage msg attrs
