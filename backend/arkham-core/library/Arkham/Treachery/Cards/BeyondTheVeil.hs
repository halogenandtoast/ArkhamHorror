module Arkham.Treachery.Cards.BeyondTheVeil
  ( BeyondTheVeil(..)
  , beyondTheVeil
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message hiding ( DeckHasNoCards )
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheVeil :: TreacheryCard BeyondTheVeil
beyondTheVeil = treachery BeyondTheVeil Cards.beyondTheVeil

instance HasAbilities BeyondTheVeil where
  getAbilities (BeyondTheVeil x) =
    [ restrictedAbility x 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ DeckHasNoCards Timing.When You
    ]

instance RunMessage BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      canAttach <-
        selectNone $ treacheryIs Cards.beyondTheVeil <> TreacheryInThreatAreaOf
          (InvestigatorWithId iid)
      when canAttach
        $ push (AttachTreachery treacheryId (InvestigatorTarget iid))
      pure t
    UseCardAbility iid source _ 1 _ | isSource attrs source -> t <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 10 0
      , Discard $ toTarget attrs
      ]
    _ -> BeyondTheVeil <$> runMessage msg attrs
