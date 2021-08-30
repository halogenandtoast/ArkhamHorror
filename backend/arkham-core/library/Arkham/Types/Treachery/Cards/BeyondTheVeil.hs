module Arkham.Types.Treachery.Cards.BeyondTheVeil
  ( BeyondTheVeil(..)
  , beyondTheVeil
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (DeckHasNoCards)
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheVeil :: TreacheryCard BeyondTheVeil
beyondTheVeil = treachery BeyondTheVeil Cards.beyondTheVeil

instance HasAbilities env BeyondTheVeil where
  getAbilities _ _ (BeyondTheVeil x) = pure
    [ restrictedAbility x 1 (InThreatAreaOf You)
      $ ForcedAbility
      $ DeckHasNoCards Timing.When You
    ]

instance TreacheryRunner env => RunMessage env BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptInvestigators <- getSet @InvestigatorId
        (TreacheryCardCode $ toCardCode attrs)
      t <$ if iid `member` exemptInvestigators
        then push (Discard $ toTarget attrs)
        else push (AttachTreachery treacheryId (InvestigatorTarget iid))
    UseCardAbility iid source _ 1 _ | isSource attrs source -> t <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 10 0
      , Discard $ toTarget attrs
      ]
    _ -> BeyondTheVeil <$> runMessage msg attrs
