module Arkham.Treachery.Cards.Compulsion (compulsion) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Effect.Builder
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Compulsion = Compulsion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

compulsion :: TreacheryCard Compulsion
compulsion = treachery Compulsion Cards.compulsion

instance HasAbilities Compulsion where
  getAbilities (Compulsion a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnBegins #when You
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage Compulsion where
  runMessage msg t@(Compulsion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      when (any ((`elem` [Skull, Cultist, Tablet, ElderThing]) . (.face)) tokens) do
        effectWithSource (attrs.ability 1) iid do
          removeOn #nextAction
          apply $ MustPerformAbilityIfCan $ AbilityRef (toSource attrs) 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Compulsion <$> liftRunMessage msg attrs
