module Arkham.Types.Treachery.Cards.AttractingAttention
  ( attractingAttention
  , AttractingAttention(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AttractingAttention = AttractingAttention TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attractingAttention :: TreacheryCard AttractingAttention
attractingAttention = treachery AttractingAttention Cards.attractingAttention

instance TreacheryRunner env => RunMessage env AttractingAttention where
  runMessage msg t@(AttractingAttention attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      broodOfYogSothoth <- getSetList (CardCode "02255")

      t <$ pushAll
        ([ chooseOneAtATime
             iid
             [ MoveToward (EnemyTarget eid) (LocationWithId lid)
             | eid <- broodOfYogSothoth
             ]
         | notNull broodOfYogSothoth
         ]
        <> [Discard (toTarget attrs)]
        )
    _ -> AttractingAttention <$> runMessage msg attrs
