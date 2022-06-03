module Arkham.Treachery.Cards.BeyondTheVeil
  ( BeyondTheVeil(..)
  , beyondTheVeil
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (DeckHasNoCards)
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheVeil :: TreacheryCard BeyondTheVeil
beyondTheVeil = treachery BeyondTheVeil Cards.beyondTheVeil

instance HasAbilities BeyondTheVeil where
  getAbilities (BeyondTheVeil x) =
    [ restrictedAbility x 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ DeckHasNoCards Timing.When You
    ]

instance TreacheryRunner env => RunMessage BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptInvestigators <- getSet @InvestigatorId
        (TreacheryCardCode $ toCardCode attrs)
      t <$ if iid `member` exemptInvestigators
        then pure ()
        else push (AttachTreachery treacheryId (InvestigatorTarget iid))
    UseCardAbility iid source _ 1 _ | isSource attrs source -> t <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 10 0
      , Discard $ toTarget attrs
      ]
    _ -> BeyondTheVeil <$> runMessage msg attrs
