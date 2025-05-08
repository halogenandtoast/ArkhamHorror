module Arkham.Act.Cards.TheReallyBadOnesV2 (theReallyBadOnesV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Name
import Arkham.Trait

newtype TheReallyBadOnesV2 = TheReallyBadOnesV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theReallyBadOnesV2 :: ActCard TheReallyBadOnesV2
theReallyBadOnesV2 = act (2, A) TheReallyBadOnesV2 Cards.theReallyBadOnesV2 Nothing

instance HasModifiersFor TheReallyBadOnesV2 where
  getModifiersFor (TheReallyBadOnesV2 attrs) = do
    modifySelect attrs UnrevealedLocation [TraitRestrictedModifier ArkhamAsylum Blank]

instance RunMessage TheReallyBadOnesV2 where
  runMessage msg a@(TheReallyBadOnesV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      danielsCell <- getJustLocationByName ("Patient Confinement" <:> "Daniel's Cell")
      createEnemyAt_ Enemies.danielChesterfield danielsCell
      advanceActDeck attrs
      pure a
    _ -> TheReallyBadOnesV2 <$> liftRunMessage msg attrs
