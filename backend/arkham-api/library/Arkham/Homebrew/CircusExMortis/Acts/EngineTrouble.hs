module Arkham.Homebrew.CircusExMortis.Acts.EngineTrouble (engineTrouble) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.ArrivingAt
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.NowArriving (Arrival (..))
import Arkham.Matcher
import Arkham.Trait

newtype EngineTrouble = EngineTrouble ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineTrouble :: ActCard EngineTrouble
engineTrouble = act (3, A) EngineTrouble Cards.engineTrouble Nothing

instance HasAbilities EngineTrouble where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (notExists $ EnemyWithTrait DarkYoung) $ Objective $ forced $ RoundEnds #when

instance RunMessage EngineTrouble where
  runMessage msg a@(EngineTrouble attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      arrivingAt attrs ArrivingAtDenver putCircusTrainIntoPlay
      advanceActDeck attrs
      pure a
    _ -> EngineTrouble <$> liftRunMessage msg attrs
