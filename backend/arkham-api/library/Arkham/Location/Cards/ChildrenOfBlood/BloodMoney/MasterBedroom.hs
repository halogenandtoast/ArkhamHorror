module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.MasterBedroom (masterBedroom) where

import Arkham.Card
import Arkham.Cost
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.History (History (historyTreacheriesDrawn))
import Arkham.History.Types
import Arkham.Keyword qualified as Keyword
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase

newtype MasterBedroom = MasterBedroom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroom :: LocationCard MasterBedroom
masterBedroom = symbolLabel $ location MasterBedroom Cards.masterBedroom 4 (PerPlayer 2)

instance HasModifiersFor MasterBedroom where
  getModifiersFor (MasterBedroom a) =
    if a.revealed
      then do
        phase <- getPhase
        history <- fmap fold . traverse (getHistory PhaseHistory) =<< select (investigatorAt a)
        when (phase == MythosPhase && length (historyTreacheriesDrawn history) == 1) do
          cards <- findAllCards (`cardMatch` CardWithType TreacheryType)
          modifyEach a (map (CardIdTarget . toCardId) cards) [AddKeyword Keyword.Surge]
      else modifySelf a [AdditionalCostToEnter $ GroupClueCost (PerPlayer 3) YourLocation]

instance HasAbilities MasterBedroom where
  getAbilities (MasterBedroom a) = extendRevealed a []

instance RunMessage MasterBedroom where
  runMessage msg (MasterBedroom attrs) = runQueueT $ MasterBedroom <$> liftRunMessage msg attrs
