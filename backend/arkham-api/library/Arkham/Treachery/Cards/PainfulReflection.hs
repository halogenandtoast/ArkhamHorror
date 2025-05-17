module Arkham.Treachery.Cards.PainfulReflection (painfulReflection) where

import Arkham.Ability
import Arkham.Helpers.ChaosToken
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PainfulReflection = PainfulReflection TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painfulReflection :: TreacheryCard PainfulReflection
painfulReflection = treachery PainfulReflection Cards.painfulReflection

instance HasAbilities PainfulReflection where
  getAbilities (PainfulReflection a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ PlayEvent #when You AnyEvent]

instance RunMessage PainfulReflection where
  runMessage msg t@(PainfulReflection attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (getPlayedEvent -> eventId) _ -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure $ PainfulReflection (setMeta eventId attrs)
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      when (any (`elem` [#skull, #cultist, #tablet, #elderthing, #autofail]) chaosTokenFaces) do
        for_ (maybeResult attrs.meta) cancelEvent
        assignHorror iid (attrs.ability 1) 1
        toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PainfulReflection <$> liftRunMessage msg attrs
