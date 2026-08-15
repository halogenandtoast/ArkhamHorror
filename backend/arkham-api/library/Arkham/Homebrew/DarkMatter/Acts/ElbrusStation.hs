module Arkham.Homebrew.DarkMatter.Acts.ElbrusStation (elbrusStation) where

import Arkham.Act.Import.Lifted
import Arkham.Card (toCard)
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.Helpers (getImpendingDoom, placeCardsFacedownEvenly)
import Arkham.Message.Lifted.Choose

newtype ElbrusStation = ElbrusStation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

elbrusStation :: ActCard ElbrusStation
elbrusStation = act (1, A) ElbrusStation Cards.elbrusStation (groupClueCost $ PerPlayer 3)

instance RunMessage ElbrusStation where
  runMessage msg a@(ElbrusStation attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      impendingDoom <- getImpendingDoom
      if impendingDoom >= 3
        then do
          shuffleEncounterDiscardBackIn
          doStep 1 msg
        else do
          lead <- getLead
          investigators <- allInvestigators
          erwin <- getSetAsideCard Assets.erwinSimmonsQuantumPhysicist
          chooseOrRunOneM lead $ targets investigators (`takeControlOfSetAsideAsset` erwin)
          advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      erwin <- getSetAsideCard Assets.erwinSimmonsFading
      Deck encounterDeck <- getEncounterDeck
      let (top, rest) = splitAt 3 encounterDeck
      setEncounterDeck $ Deck rest
      investigators <- allInvestigators
      placeCardsFacedownEvenly investigators (erwin : map toCard top)
      advanceToAct attrs Cards.destabilization A
      pure a
    _ -> ElbrusStation <$> liftRunMessage msg attrs
