module Arkham.Scenarios.WhereTheGodsDwell.Helpers where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Helpers.Message (toDiscardBy)
import Arkham.Id
import Arkham.Location.Types (LocationAttrs (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Types (Field (..))

whichWhisperingChaos :: LocationAttrs -> CardDef
whichWhisperingChaos attrs = case attrs.label of
  "northTower" -> Treacheries.whisperingChaosNorth
  "southTower" -> Treacheries.whisperingChaosSouth
  "eastTower" -> Treacheries.whisperingChaosEast
  "westTower" -> Treacheries.whisperingChaosWest
  _ -> error "Invalid Label"

forsakenTowerCriteria :: LocationAttrs -> Criterion
forsakenTowerCriteria attrs =
  exists (TreacheryInHandOf You <> treacheryIs (whichWhisperingChaos attrs))
    <> exists (EnemyInHandOf You <> EnemyWithTitle "Nyarlathotep")

forsakenTowerAbilities :: LocationAttrs -> [Ability]
forsakenTowerAbilities attrs = [restrictedAbility attrs 1 (forsakenTowerCriteria attrs) actionAbility]

getWhisperingChaos :: HasGame m => LocationAttrs -> m TreacheryId
getWhisperingChaos attrs = selectJust $ treacheryIs (whichWhisperingChaos attrs)

revealWhisperingChaos :: (HasQueue Message m, HasGame m) => LocationAttrs -> m ()
revealWhisperingChaos attrs = do
  x <- getWhisperingChaos attrs
  card <- field TreacheryCard x
  push $ RevealCard (toCardId card)

discardWhisperingChaos :: (HasQueue Message m, HasGame m) => LocationAttrs -> m ()
discardWhisperingChaos attrs = do
  x <- getWhisperingChaos attrs
  iid <- selectJust $ InvestigatorWithTreacheryInHand $ TreacheryWithId x
  push $ toDiscardBy iid attrs x

shuffleWhisperingChaosBackIntoEncounterDeck
  :: (HasQueue Message m, HasGame m) => LocationAttrs -> m ()
shuffleWhisperingChaosBackIntoEncounterDeck attrs = do
  x <- getWhisperingChaos attrs
  push $ ShuffleBackIntoEncounterDeck (toTarget x)

-- While this card is in your hand, you may activate the ability on the Northern Tower if you reveal this card from your hand. If the ability on the Northern Tower resolves successfully, discard this card. If it fails, shuffle this card back into the encounter deck.
