module Arkham.Types.Treachery.Cards.DissonantVoices where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

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
