module Arkham.Treachery.Cards.NobodysHome (nobodysHome, NobodysHome (..)) where

import Arkham.Ability
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NobodysHome = NobodysHome TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nobodysHome :: TreacheryCard NobodysHome
nobodysHome = treachery NobodysHome Cards.nobodysHome

instance HasModifiersFor NobodysHome where
  getModifiersFor (AbilityTarget _ ab) (NobodysHome attrs) = maybeModified attrs do
    lid <- hoistMaybe ab.source.location
    guard $ toTarget lid `elem` attrs.attached
    guard $ #investigate `elem` ab.actions
    pure [AdditionalCost (ActionCost 1)]
  getModifiersFor _ _ = pure []

instance HasAbilities NobodysHome where
  getAbilities (NobodysHome a) = case a.attached of
    Just (LocationTarget lid) ->
      [restrictedAbility a 1 (exists $ be lid <> LocationWithoutClues) $ ForcedAbility AnyWindow]
    _ -> []

instance RunMessage NobodysHome where
  runMessage msg t@(NobodysHome attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        whenM (fieldNone LocationClues lid) $ gainSurge attrs
        attachTreachery attrs lid
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> NobodysHome <$> liftRunMessage msg attrs
