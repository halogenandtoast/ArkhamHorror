module Arkham.Types.Treachery.Cards.StalkedInTheDark
  ( stalkedInTheDark
  , StalkedInTheDark(..)
  ) where

import Arkham.Prelude

import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype StalkedInTheDark = StalkedInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedInTheDark :: TreacheryCard StalkedInTheDark
stalkedInTheDark = treachery StalkedInTheDark Cards.stalkedInTheDark

instance TreacheryRunner env => RunMessage env StalkedInTheDark where
  runMessage msg t@(StalkedInTheDark attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mHuntingHorrorId <- getHuntingHorror
      case mHuntingHorrorId of
        Just eid -> do
          lid <- getId @LocationId iid
          iids <- getSetList @InvestigatorId lid
          t <$ pushAll
            ([Ready (EnemyTarget eid), EnemyEngageInvestigator eid iid]
            <> [ EnemyAttack iid' eid DamageAny | iid' <- iids ]
            )
        Nothing -> t <$ push (Surge iid source)
    _ -> StalkedInTheDark <$> runMessage msg attrs
