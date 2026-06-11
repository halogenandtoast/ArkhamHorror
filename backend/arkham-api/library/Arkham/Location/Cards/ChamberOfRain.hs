module Arkham.Location.Cards.ChamberOfRain (chamberOfRain) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChamberOfRain = ChamberOfRain LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfRain :: LocationCard ChamberOfRain
chamberOfRain = location ChamberOfRain Cards.chamberOfRain 1 (PerPlayer 1)

-- The Chamber of Rain is sealed shut while the water flows: while it is act
-- 1, investigators cannot move into or out of this location.
instance HasModifiersFor ChamberOfRain where
  getModifiersFor (ChamberOfRain a) = do
    step <- getCurrentActStep
    modifySelfWhen a (step == 1) [Blocked]
    modifySelectWhen a (step == 1) (investigatorAt a.id) [CannotMove]

instance HasAbilities ChamberOfRain where
  getAbilities (ChamberOfRain a) =
    extendRevealed1 a
      $ restricted a 1 (OnAct 1)
      $ forced
      $ DrawCard #when (investigatorAt a.id) (basic #enemy) EncounterDeck

instance RunMessage ChamberOfRain where
  runMessage msg l@(ChamberOfRain attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      quietCancelCardDraw card
      case card of
        EncounterCard ec -> push $ AddToEncounterDiscard ec
        _ -> pure ()
      drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> ChamberOfRain <$> liftRunMessage msg attrs
