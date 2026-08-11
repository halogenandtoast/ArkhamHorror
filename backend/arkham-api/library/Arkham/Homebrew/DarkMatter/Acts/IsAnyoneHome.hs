module Arkham.Homebrew.DarkMatter.Acts.IsAnyoneHome (isAnyoneHome) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (ScanResult (..), scanEvent)
import Arkham.Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype IsAnyoneHome = IsAnyoneHome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isAnyoneHome :: ActCard IsAnyoneHome
isAnyoneHome = act (1, A) IsAnyoneHome Cards.isAnyoneHome Nothing

-- "Objective - When you draw a story asset from the scanning deck, advance."
instance HasAbilities IsAnyoneHome where
  getAbilities (IsAnyoneHome a) =
    [mkAbility a 1 $ Objective $ forced $ ScenarioEvent #after Nothing scanEvent]

getScanResult :: [Window] -> Maybe ScanResult
getScanResult = \case
  (windowType -> Window.ScenarioEvent key _ v) : _ | key == scanEvent -> Just (toResult v)
  _ : rest -> getScanResult rest
  [] -> Nothing

instance RunMessage IsAnyoneHome where
  runMessage msg a@(IsAnyoneHome attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getScanResult -> Just r) _
      | maybe False ((== AssetType) . toCardType) (scannedCard r) -> do
          advanceVia #other attrs attrs
          pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> IsAnyoneHome <$> liftRunMessage msg attrs
