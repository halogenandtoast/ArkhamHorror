module Arkham.Treachery.Cards.TerrorGate (terrorGate) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Scenario (getScenarioMeta)
import Arkham.Matcher
import Arkham.Scenarios.TheMidwinterGala.Faction
import Arkham.Scenarios.TheMidwinterGala.Meta
import Arkham.Trait (Trait (Rival))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorGate = TerrorGate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorGate :: TreacheryCard TerrorGate
terrorGate = treachery TerrorGate Cards.terrorGate

instance RunMessage TerrorGate where
  runMessage msg t@(TerrorGate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 2, IfEnemyExistsCalculation (EnemyWithTrait Rival) (Fixed 2) (Fixed 0)]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      Meta {ally} <- getScenarioMeta @Meta
      case ally of
        TheFoundation -> assignDamage iid attrs 2
        MiskatonicUniversity -> chooseAndDiscardCards iid attrs 2
        TheSyndicate -> loseResources iid attrs 3
        TheSilverTwilightLodge -> assignHorror iid attrs 2
        LocalsOfKingsport -> loseActions iid attrs 1
      pure t
    _ -> TerrorGate <$> liftRunMessage msg attrs
