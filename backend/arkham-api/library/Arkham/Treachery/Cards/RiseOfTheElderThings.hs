module Arkham.Treachery.Cards.RiseOfTheElderThings (riseOfTheElderThings) where

import Arkham.Card
import Arkham.Helpers.Message hiding (gainSurge)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (ElderThing))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RiseOfTheElderThings = RiseOfTheElderThings TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseOfTheElderThings :: TreacheryCard RiseOfTheElderThings
riseOfTheElderThings = treachery RiseOfTheElderThings Cards.riseOfTheElderThings

instance RunMessage RiseOfTheElderThings where
  runMessage msg t@(RiseOfTheElderThings attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      elderThings <-
        filterCards (card_ $ #enemy <> CardWithTrait ElderThing) <$> scenarioField ScenarioDiscard
      when (null elderThings) $ gainSurge attrs
      unless (null elderThings) do
        focusCards elderThings \unfocus -> do
          chooseOneM iid do
            for_ elderThings \elderThing -> do
              targeting elderThing $ pushM $ createEnemyWithPlacement_ (toCard elderThing) (InThreatArea iid)
          push unfocus
      selectEach (enemyEngagedWith iid <> withTrait ElderThing) \x -> do
        roundModifiers attrs x [EnemyFight 1, EnemyEvade 1]
      pure t
    _ -> RiseOfTheElderThings <$> liftRunMessage msg attrs
