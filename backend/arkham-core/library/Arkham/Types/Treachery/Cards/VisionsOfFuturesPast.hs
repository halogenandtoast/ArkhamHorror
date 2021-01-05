module Arkham.Types.Treachery.Cards.VisionsOfFuturesPast
  ( VisionsOfFuturesPast(..)
  , visionsOfFuturesPast
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype VisionsOfFuturesPast = VisionsOfFuturesPast Attrs
  deriving newtype (Show, ToJSON, FromJSON)

visionsOfFuturesPast :: TreacheryId -> a -> VisionsOfFuturesPast
visionsOfFuturesPast uuid _ = VisionsOfFuturesPast $ baseAttrs uuid "02083"

instance HasModifiersFor env VisionsOfFuturesPast where
  getModifiersFor = noModifiersFor

instance HasActions env VisionsOfFuturesPast where
  getActions i window (VisionsOfFuturesPast attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env VisionsOfFuturesPast where
  runMessage msg t@(VisionsOfFuturesPast attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessages
        [ RevelationSkillTest iid source SkillWillpower 5
        , Discard (TreacheryTarget treacheryId)
        ]
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} n
      | tid == treacheryId -> t
      <$ unshiftMessage (DiscardTopOfDeck iid n Nothing)
    _ -> VisionsOfFuturesPast <$> runMessage msg attrs
