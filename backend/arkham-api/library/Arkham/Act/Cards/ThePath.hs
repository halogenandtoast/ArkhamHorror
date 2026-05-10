module Arkham.Act.Cards.ThePath (thePath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.BeyondTheGatesOfSleep.Helpers

newtype ThePath = ThePath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePath :: ActCard ThePath
thePath = act (4, A) ThePath Cards.thePath (Just $ GroupClueCost (PerPlayer 5) "The Enchanted Path")

instance HasAbilities ThePath where
  getAbilities (ThePath x) =
    extend1 x
      $ restricted x 1 (notYetRecorded TheDreamersStrayedFromThePath)
      $ forced
      $ Enters #after Anyone
      $ LocationWithUnrevealedTitle "Enchanted Woods"

instance RunMessage ThePath where
  runMessage msg a@(ThePath attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record TheDreamersStrayedFromThePath
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      leadChooseOneM $ scenarioI18n $ scope "thePath" do
        labeled' "stepBack" $ push R1
        labeled' "interrupt" $ push R2

      pure a
    _ -> ThePath <$> liftRunMessage msg attrs
