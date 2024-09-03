module Arkham.Treachery.Cards.DissonantVoices where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype DissonantVoices = DissonantVoices TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissonantVoices :: TreacheryCard DissonantVoices
dissonantVoices = treachery DissonantVoices Cards.dissonantVoices

instance HasModifiersFor DissonantVoices where
  getModifiersFor (InvestigatorTarget iid) (DissonantVoices attrs) =
    modified
      attrs
      [ CannotPlay (CardWithOneOf [#asset, #event])
      | treacheryInThreatArea iid attrs
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities DissonantVoices where
  getAbilities (DissonantVoices a) =
    [restrictedAbility a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage DissonantVoices where
  runMessage msg t@(DissonantVoices attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DissonantVoices <$> runMessage msg attrs
