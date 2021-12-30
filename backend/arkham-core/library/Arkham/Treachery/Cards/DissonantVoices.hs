module Arkham.Treachery.Cards.DissonantVoices where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Attrs
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype DissonantVoices= DissonantVoices TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissonantVoices :: TreacheryCard DissonantVoices
dissonantVoices = treachery DissonantVoices Cards.dissonantVoices

instance HasModifiersFor env DissonantVoices where
  getModifiersFor _ (InvestigatorTarget iid) (DissonantVoices attrs) =
    pure $ toModifiers
      attrs
      [ CannotPlay [(AssetType, mempty), (EventType, mempty)]
      | treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities DissonantVoices where
  getAbilities (DissonantVoices a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ RoundEnds
        Timing.When
    ]

instance TreacheryRunner env => RunMessage env DissonantVoices where
  runMessage msg t@(DissonantVoices attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId t) (InvestigatorTarget iid))
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> DissonantVoices <$> runMessage msg attrs
