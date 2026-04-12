module Arkham.Treachery.Cards.UnbrokenWeb (unbrokenWeb) where

import Arkham.Ability
import Arkham.Effect.Builder
import Arkham.Helpers.Discover
import Arkham.Matcher
import Arkham.Message.Lifted.Discover
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnbrokenWeb = UnbrokenWeb TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unbrokenWeb :: TreacheryCard UnbrokenWeb
unbrokenWeb = treachery UnbrokenWeb Cards.unbrokenWeb

instance HasAbilities UnbrokenWeb where
  getAbilities (UnbrokenWeb a) = mapFold a.owner \iid ->
    [ restricted a 1 OnSameLocation $ freeReaction (WouldDiscoverClues #when You YourLocation $ atLeast 1)
    , mkAbility a 2 $ forcedOnElimination iid
    ]

instance RunMessage UnbrokenWeb where
  runMessage msg t@(UnbrokenWeb attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      insteadOfDiscoveringClues iid
        $ getDiscoveredTotal iid
        >=> placeTokens (attrs.ability 1) attrs #horror
      doStep 1 msg
      pure t
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      when (attrs.token #horror >= 4) $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ attrs.owner \iid -> do
        withSource (attrs.ability 2) $ effect iid do
          during #resolution
          apply $ XPModifier "Unbroken Web" (-2)
      pure t
    _ -> UnbrokenWeb <$> liftRunMessage msg attrs
