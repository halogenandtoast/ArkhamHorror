module Arkham.Scenarios.WhereTheGodsDwell.Helpers where

import Arkham.Ability
import Arkham.Card.CardDef
import Arkham.Location.Types (LocationAttrs (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

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
