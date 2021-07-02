module Arkham.Types.Treachery.Cards.VisionsOfFuturesPast
  ( VisionsOfFuturesPast(..)
  , visionsOfFuturesPast
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype VisionsOfFuturesPast = VisionsOfFuturesPast TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsOfFuturesPast :: TreacheryCard VisionsOfFuturesPast
visionsOfFuturesPast = treachery VisionsOfFuturesPast Cards.visionsOfFuturesPast

instance HasModifiersFor env VisionsOfFuturesPast where
  getModifiersFor = noModifiersFor

instance HasActions env VisionsOfFuturesPast where
  getActions i window (VisionsOfFuturesPast attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env VisionsOfFuturesPast where
  runMessage msg t@(VisionsOfFuturesPast attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillWillpower 5
      , Discard (TreacheryTarget treacheryId)
      ]
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t
      <$ unshiftMessage (DiscardTopOfDeck iid n Nothing)
    _ -> VisionsOfFuturesPast <$> runMessage msg attrs
