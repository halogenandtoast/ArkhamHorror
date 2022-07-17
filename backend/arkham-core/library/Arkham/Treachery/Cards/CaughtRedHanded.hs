module Arkham.Treachery.Cards.CaughtRedHanded
  ( caughtRedHanded
  , CaughtRedHanded(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CaughtRedHanded = CaughtRedHanded TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtRedHanded :: TreacheryCard CaughtRedHanded
caughtRedHanded = treachery CaughtRedHanded Cards.caughtRedHanded

instance RunMessage CaughtRedHanded where
  runMessage msg t@(CaughtRedHanded attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      enemies <- selectListMap EnemyTarget $ EnemyAt
        (locationWithInvestigator iid
        <> ConnectedFrom (locationWithInvestigator iid)
        )
      hunters <- selectListMap EnemyTarget $ HunterEnemy <> EnemyAt
        (ConnectedFrom $ locationWithInvestigator iid)
      pushAll
        $ map Ready enemies
        <> [ MoveToward target (locationWithInvestigator iid)
           | target <- hunters
           ]
        <> if null hunters
             then [ShuffleIntoDeck iid (toTarget attrs)]
             else [Discard $ toTarget attrs]

      pure t
    _ -> CaughtRedHanded <$> runMessage msg attrs
