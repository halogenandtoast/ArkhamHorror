module Arkham.Treachery.Cards.DissonantVoices (dissonantVoices) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DissonantVoices = DissonantVoices TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissonantVoices :: TreacheryCard DissonantVoices
dissonantVoices = treachery DissonantVoices Cards.dissonantVoices

instance HasModifiersFor DissonantVoices where
  getModifiersFor (DissonantVoices attrs) =
    inThreatAreaGets attrs [CannotPlay (CardWithOneOf [#asset, #event])]

instance HasAbilities DissonantVoices where
  getAbilities (DissonantVoices a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage DissonantVoices where
  runMessage msg t@(DissonantVoices attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DissonantVoices <$> liftRunMessage msg attrs
