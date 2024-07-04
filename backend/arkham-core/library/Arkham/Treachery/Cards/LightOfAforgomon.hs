module Arkham.Treachery.Cards.LightOfAforgomon (LightOfAforgomon (..), lightOfAforgomon) where

import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype LightOfAforgomon = LightOfAforgomon TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightOfAforgomon :: TreacheryCard LightOfAforgomon
lightOfAforgomon = treachery LightOfAforgomon Cards.lightOfAforgomon

instance HasModifiersFor LightOfAforgomon where
  getModifiersFor (InvestigatorTarget _) (LightOfAforgomon attrs) =
    pure $ toModifiers attrs [TreatAllDamageAsDirect]
  getModifiersFor _ _ = pure []

instance RunMessage LightOfAforgomon where
  runMessage msg t@(LightOfAforgomon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      targetActs <- selectTargets $ NotAct $ ActWithTreachery $ treacheryIs Cards.lightOfAforgomon
      targetAgendas <-
        selectTargets $ NotAgenda $ AgendaWithTreachery $ treacheryIs Cards.lightOfAforgomon
      when (notNull $ targetActs <> targetAgendas) do
        chooseOne
          iid
          [ TargetLabel target [Msg.attachTreachery attrs target]
          | target <- targetActs <> targetAgendas
          ]
      pure t
    _ -> LightOfAforgomon <$> liftRunMessage msg attrs
