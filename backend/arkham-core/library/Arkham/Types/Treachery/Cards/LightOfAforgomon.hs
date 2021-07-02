module Arkham.Types.Treachery.Cards.LightOfAforgomon
  ( LightOfAforgomon(..)
  , lightOfAforgomon
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype LightOfAforgomon = LightOfAforgomon TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightOfAforgomon :: TreacheryCard LightOfAforgomon
lightOfAforgomon = treachery LightOfAforgomon Cards.lightOfAforgomon

instance HasModifiersFor env LightOfAforgomon where
  getModifiersFor _ (InvestigatorTarget _) (LightOfAforgomon attrs) =
    pure $ toModifiers attrs [TreatAllDamageAsDirect]
  getModifiersFor _ _ _ = pure []

instance HasActions env LightOfAforgomon where
  getActions i window (LightOfAforgomon attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env LightOfAforgomon where
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
      if null (targetActs <> targetAgendas)
        then unshiftMessage (Discard $ toTarget attrs)
        else unshiftMessage $ chooseOne
          iid
          [ AttachTreachery treacheryId target
          | target <- targetActs <> targetAgendas
          ]
      LightOfAforgomon <$> runMessage msg attrs
    _ -> LightOfAforgomon <$> runMessage msg attrs
