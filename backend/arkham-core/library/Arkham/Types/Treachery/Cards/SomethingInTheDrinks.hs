module Arkham.Types.Treachery.Cards.SomethingInTheDrinks
  ( SomethingInTheDrinks(..)
  , somethingInTheDrinks
  )
where


import Arkham.Types.Game.Helpers
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SomethingInTheDrinks = SomethingInTheDrinks TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingInTheDrinks :: TreacheryId -> a -> SomethingInTheDrinks
somethingInTheDrinks uuid _ = SomethingInTheDrinks $ baseAttrs uuid "02081"

instance HasModifiersFor env SomethingInTheDrinks where
  getModifiersFor = noModifiersFor

instance HasActions env SomethingInTheDrinks where
  getActions i window (SomethingInTheDrinks attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SomethingInTheDrinks where
  runMessage msg t@(SomethingInTheDrinks attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      scenarioLogs <- getSet ()
      investigatorIds <- getInvestigatorIds
      t <$ unshiftMessages
        ([ LoseActions iid source 1
         | iid <- investigatorIds
         , HadADrink iid `member` scenarioLogs
         ]
        <> [Continue "Continue", Discard $ toTarget attrs]
        )
    _ -> SomethingInTheDrinks <$> runMessage msg attrs
