module Arkham.Homebrew.DarkMatter.Acts.Reconnected (reconnected) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (ScanResult (..))
import Arkham.LocationSymbol qualified as LS
import Arkham.Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype Reconnected = Reconnected ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reconnected :: ActCard Reconnected
reconnected = act (3, A) Reconnected Cards.reconnected Nothing

{- | "After an investigator at the Cryosleep Quarters performs a scan matching
the icon below, regardless of whether it is successful or not, you may
advance." The window matcher narrows to scans made at the Cryosleep Quarters;
the icon itself lives in the "scan" ScenarioEvent payload, which no window
matcher can inspect, so it is checked in the handler.
-}
instance HasAbilities Reconnected where
  getAbilities (Reconnected a) =
    [ mkAbility a 1
        $ freeReaction
        $ ScenarioEvent
          #after
          (Just $ InvestigatorAt $ locationIs Locations.cryosleepQuarters)
          "scan"
    ]

getScanResult :: [Window] -> Maybe ScanResult
getScanResult = \case
  [] -> Nothing
  ((windowType -> Window.ScenarioEvent "scan" _ v) : _) -> Just (toResult v)
  (_ : xs) -> getScanResult xs

instance RunMessage Reconnected where
  runMessage msg a@(Reconnected attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getScanResult -> Just r) _ -> do
      when (LS.Trefoil `elem` scannedFor r) $ advanceActDeck attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> Reconnected <$> liftRunMessage msg attrs
