module Arkham.Asset.Assets.OldMemory (oldMemory) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FateOfTheVale.Helpers (scenarioI18n)
import Arkham.Window (Window, getBatchId, windowType)
import Arkham.Window qualified as Window

newtype OldMemory = OldMemory AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldMemory :: AssetCard OldMemory
oldMemory = asset OldMemory Cards.oldMemory

instance HasAbilities OldMemory where
  getAbilities (OldMemory a) =
    [ controlled_ a 1
        $ triggered
          ( oneOf
              [ ScenarioEvent #when Nothing "wouldDrawFromAbyss:mythos"
              , ScenarioEvent #when Nothing "wouldDrawFromAbyss:scenario"
              ]
          )
          (GroupClueCost (PerPlayer 1) Anywhere <> RemoveCost (toTarget a))
    ]

{- | The drawn card and whether it came from the Mythos draw (an encounter card
resolved as a normal draw) or a scenario draw (resolved by the scenario, e.g.
placing a location or triggering a Resident attack).
-}
getAbyssDraw :: [Window] -> (Bool, Card)
getAbyssDraw = \case
  [] -> error "Old Memory: missing wouldDrawFromAbyss window"
  ((windowType -> Window.ScenarioEvent key _ value) : _)
    | key == "wouldDrawFromAbyss:mythos" -> (True, toResult value)
    | key == "wouldDrawFromAbyss:scenario" -> (False, toResult value)
  (_ : xs) -> getAbyssDraw xs

instance RunMessage OldMemory where
  runMessage msg a@(OldMemory attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      let (isMythos, card) = getAbyssDraw ws
      cancelBatch (getBatchId ws)
      investigators <- select Anyone
      scenarioI18n $ chooseOneM iid do
        labeled' "oldMemory.chooseInvestigator" do
          chooseOneM iid $ targets investigators \iid' ->
            if isMythos
              then drawCard iid' card
              else scenarioSpecific "drawFromAbyss" (iid', card)
        labeled' "oldMemory.cancelRevelation" $ removeCardFromGame card
      pure a
    _ -> OldMemory <$> liftRunMessage msg attrs
