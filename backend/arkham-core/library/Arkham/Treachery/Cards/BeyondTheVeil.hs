module Arkham.Treachery.Cards.BeyondTheVeil (
  BeyondTheVeil (..),
  beyondTheVeil,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

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
        selectNone
          $ treacheryIs Cards.beyondTheVeil
          <> TreacheryInThreatAreaOf
            (InvestigatorWithId iid)
      when canAttach
        $ push (AttachTreachery treacheryId (InvestigatorTarget iid))
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ assignDamage iid source 10
        , toDiscardBy iid (toAbilitySource attrs 2) attrs
        ]
      pure t
    _ -> BeyondTheVeil <$> runMessage msg attrs
