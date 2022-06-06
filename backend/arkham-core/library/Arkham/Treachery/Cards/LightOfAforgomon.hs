module Arkham.Treachery.Cards.LightOfAforgomon
  ( LightOfAforgomon(..)
  , lightOfAforgomon
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype LightOfAforgomon = LightOfAforgomon TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightOfAforgomon :: TreacheryCard LightOfAforgomon
lightOfAforgomon = treachery LightOfAforgomon Cards.lightOfAforgomon

instance HasModifiersFor LightOfAforgomon where
  getModifiersFor _ (InvestigatorTarget _) (LightOfAforgomon attrs) =
    pure $ toModifiers attrs [TreatAllDamageAsDirect]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage LightOfAforgomon where
  runMessage msg (LightOfAforgomon attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptActs <- getSet (TreacheryCardCode $ CardCode "81025")
      exemptAgendas <- getSet (TreacheryCardCode $ CardCode "81025")
      targetActs <-
        map ActTarget . setToList . (`difference` exemptActs) <$> getSet ()
      targetAgendas <-
        map AgendaTarget
        . setToList
        . (`difference` exemptAgendas)
        <$> getSet ()
      when
        (notNull $ targetActs <> targetAgendas)
        (push $ chooseOne
          iid
          [ AttachTreachery treacheryId target
          | target <- targetActs <> targetAgendas
          ]
        )
      LightOfAforgomon <$> runMessage msg attrs
    _ -> LightOfAforgomon <$> runMessage msg attrs
