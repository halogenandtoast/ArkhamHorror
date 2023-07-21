module Arkham.Treachery.Cards.VisionsOfFuturesPast (
  VisionsOfFuturesPast (..),
  visionsOfFuturesPast,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VisionsOfFuturesPast = VisionsOfFuturesPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsOfFuturesPast :: TreacheryCard VisionsOfFuturesPast
visionsOfFuturesPast =
  treachery VisionsOfFuturesPast Cards.visionsOfFuturesPast

instance RunMessage VisionsOfFuturesPast where
  runMessage msg t@(VisionsOfFuturesPast attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source
        | isSource attrs source ->
            t <$ push (RevelationSkillTest iid source SkillWillpower 5)
      FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget {} _ n
        | tid == treacheryId -> do
            push (DiscardTopOfDeck iid n (toSource attrs) Nothing)
            pure t
      _ -> VisionsOfFuturesPast <$> runMessage msg attrs
