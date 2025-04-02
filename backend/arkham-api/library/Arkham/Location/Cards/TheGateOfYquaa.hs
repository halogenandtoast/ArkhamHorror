module Arkham.Location.Cards.TheGateOfYquaa (theGateOfYquaa) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype TheGateOfYquaa = TheGateOfYquaa LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGateOfYquaa :: LocationCard TheGateOfYquaa
theGateOfYquaa = location TheGateOfYquaa Cards.theGateOfYquaa 1 (PerPlayer 1)

instance HasModifiersFor TheGateOfYquaa where
  getModifiersFor (TheGateOfYquaa a) = do
    modifySelfWhen a a.unrevealed [Blocked, CannotHaveAttachments]

instance HasAbilities TheGateOfYquaa where
  getAbilities (TheGateOfYquaa a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist (mapOneOf InvestigatorWithActiveSeal [minBound ..])) actionAbility
      , withTooltip
          "You retreat, hoping you’ve done enough to seal away whatever lies beyond the gate. (If you control 1 or more seals, you retain control of them.)"
          $ resignAction a
      ]

instance RunMessage TheGateOfYquaa where
  runMessage msg l@(TheGateOfYquaa attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mseal <- headMay . filter (\s -> s.active) . toList <$> field InvestigatorSeals iid
      for_ mseal \(Seal kind active _) -> placeSeal attrs (Seal kind active $ Just iid)
      pure l
    _ -> TheGateOfYquaa <$> liftRunMessage msg attrs
