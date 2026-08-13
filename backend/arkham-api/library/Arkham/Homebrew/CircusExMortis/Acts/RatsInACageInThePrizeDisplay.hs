module Arkham.Homebrew.CircusExMortis.Acts.RatsInACageInThePrizeDisplay (ratsInACage_007) where

import Arkham.Act.Import.Lifted
import Arkham.Card (toCardDef)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.Helpers (lookupRatsInACage)
import Arkham.Matcher
import Arkham.Placement

newtype RatsInACageInThePrizeDisplay = RatsInACageInThePrizeDisplay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ratsInACage_007 :: ActCard RatsInACageInThePrizeDisplay
ratsInACage_007 = act (1, A) RatsInACageInThePrizeDisplay Cards.ratsInACage_007 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInThePrizeDisplay where
  runMessage msg a@(RatsInACageInThePrizeDisplay attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      for_ (lookupRatsInACage $ toCardDef attrs) \(locationDef, token) -> do
        lid <- selectJust $ locationIs locationDef
        card <- getSetAsideCard Assets.illusoryLocus
        createAssetAt_ card (AttachedToLocation lid)
        addChaosToken token
      advanceActDeck attrs
      pure a
    _ -> RatsInACageInThePrizeDisplay <$> liftRunMessage msg attrs
