module Arkham.Treachery.Cards.DarkFuture (darkFuture) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Helpers.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DarkFuture = DarkFuture TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkFuture :: TreacheryCard DarkFuture
darkFuture = treachery DarkFuture Cards.darkFuture

instance HasModifiersFor DarkFuture where
  getModifiersFor (DarkFuture a) = do
    inThreatAreaGets a
      $ map CannotCancelOrIgnoreChaosToken [ElderSign, Skull, Cultist, Tablet, ElderThing, AutoFail]

instance HasAbilities DarkFuture where
  getAbilities (DarkFuture a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #after You]

instance RunMessage DarkFuture where
  runMessage msg t@(DarkFuture attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 5
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      resetChaosTokens (attrs.ability 1)
      when (ElderSign `elem` chaosTokenFaces) $ toDiscardBy iid (attrs.ability 1) attrs
      continue_ iid
      pure t
    _ -> DarkFuture <$> liftRunMessage msg attrs
