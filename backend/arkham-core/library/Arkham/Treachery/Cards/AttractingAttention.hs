module Arkham.Treachery.Cards.AttractingAttention
  ( attractingAttention
  , AttractingAttention(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype AttractingAttention = AttractingAttention TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attractingAttention :: TreacheryCard AttractingAttention
attractingAttention = treachery AttractingAttention Cards.attractingAttention

instance TreacheryRunner env => RunMessage AttractingAttention where
  runMessage msg t@(AttractingAttention attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      broodOfYogSothoth <- getSetList (CardCode "02255")

      t <$ pushAll
        [ chooseOneAtATime
            iid
            [ MoveToward (EnemyTarget eid) (LocationWithId lid)
            | eid <- broodOfYogSothoth
            ]
        | notNull broodOfYogSothoth
        ]
    _ -> AttractingAttention <$> runMessage msg attrs
