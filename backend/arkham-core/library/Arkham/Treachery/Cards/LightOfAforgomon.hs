module Arkham.Treachery.Cards.LightOfAforgomon (
  LightOfAforgomon (..),
  lightOfAforgomon,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype LightOfAforgomon = LightOfAforgomon TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lightOfAforgomon :: TreacheryCard LightOfAforgomon
lightOfAforgomon = treachery LightOfAforgomon Cards.lightOfAforgomon

instance HasModifiersFor LightOfAforgomon where
  getModifiersFor (InvestigatorTarget _) (LightOfAforgomon attrs) =
    pure $ toModifiers attrs [TreatAllDamageAsDirect]
  getModifiersFor _ _ = pure []

instance RunMessage LightOfAforgomon where
  runMessage msg (LightOfAforgomon attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      targetActs <-
        selectListMap ActTarget
          $ NotAct
          $ ActWithTreachery
          $ treacheryIs
            Cards.lightOfAforgomon
      targetAgendas <-
        selectListMap AgendaTarget
          $ NotAgenda
          $ AgendaWithTreachery
          $ treacheryIs Cards.lightOfAforgomon
      player <- getPlayer iid
      pushWhen (notNull $ targetActs <> targetAgendas)
        $ chooseOne
          player
          [ TargetLabel target [AttachTreachery treacheryId target]
          | target <- targetActs <> targetAgendas
          ]
      LightOfAforgomon <$> runMessage msg attrs
    _ -> LightOfAforgomon <$> runMessage msg attrs
