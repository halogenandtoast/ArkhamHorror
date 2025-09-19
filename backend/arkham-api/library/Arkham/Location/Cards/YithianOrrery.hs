module Arkham.Location.Cards.YithianOrrery (yithianOrrery) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype YithianOrrery = YithianOrrery LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianOrrery :: LocationCard YithianOrrery
yithianOrrery = location YithianOrrery Cards.yithianOrrery 4 (PerPlayer 1)

instance HasModifiersFor YithianOrrery where
  getModifiersFor (YithianOrrery a) = modifySelect a (investigatorAt (toId a)) [HandSize 2]

instance HasAbilities YithianOrrery where
  getAbilities (YithianOrrery a) =
    extendRevealed1 a $ restricted a 1 (Here <> NoCluesOnThis) doubleActionAbility

instance RunMessage YithianOrrery where
  runMessage msg l@(YithianOrrery attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember RealizedWhatYearItIs
      pure l
    _ -> YithianOrrery <$> liftRunMessage msg attrs
