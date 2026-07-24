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
              , ScenarioEvent #when Nothing "wouldRevealFromAbyss"
              ]
          )
          (GroupClueCost (PerPlayer 1) Anywhere <> RemoveCost (toTarget a))
    ]

data AbyssCardEvent = MythosDraw | ScenarioDraw | Reveal

{- | The affected card and how it was encountered. Mythos draws resolve as a
normal encounter draw; scenario draws and cards intercepted while being
revealed use Fate of the Vale's scenario-specific draw handling.
-}
getAbyssCardEvent :: [Window] -> (AbyssCardEvent, Card)
getAbyssCardEvent = \case
  [] -> error "Old Memory: missing Abyss card window"
  ((windowType -> Window.ScenarioEvent key _ value) : _)
    | key == "wouldDrawFromAbyss:mythos" -> (MythosDraw, toResult value)
    | key == "wouldDrawFromAbyss:scenario" -> (ScenarioDraw, toResult value)
    | key == "wouldRevealFromAbyss" -> (Reveal, toResult value)
  (_ : xs) -> getAbyssCardEvent xs

instance RunMessage OldMemory where
  runMessage msg a@(OldMemory attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      let (event, card) = getAbyssCardEvent ws
      cancelBatch (getBatchId ws)
      when (case event of Reveal -> True; _ -> False)
        $ scenarioSpecific "removeFromAbyss" (toCardId card)
      investigators <- select Anyone
      highlightCards [card]
      scenarioI18n $ chooseOneM iid do
        labeled' "oldMemory.chooseInvestigator" do
          chooseOneM iid $ targets investigators \iid' -> do
            push $ ObtainCard (toCardId card)
            highlightCards ([] :: [Card])
            case event of
              MythosDraw -> drawCard iid' card
              ScenarioDraw -> scenarioSpecific "drawFromAbyss" (iid', card)
              Reveal -> scenarioSpecific "drawFromAbyss" (iid', card)
        labeled' "oldMemory.cancelRevelation" do
          highlightCards ([] :: [Card])
          removeCardFromGame card
      pure a
    _ -> OldMemory <$> liftRunMessage msg attrs
