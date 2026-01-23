module Arkham.Treachery.Cards.ChromaBlight (chromaBlight) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ChromaBlight = ChromaBlight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chromaBlight :: TreacheryCard ChromaBlight
chromaBlight = treachery ChromaBlight Cards.chromaBlight

instance HasAbilities ChromaBlight where
  getAbilities (ChromaBlight a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ DrawsCards #after You AnyCards (atLeast 1)
    , restricted a 2 (InThreatAreaOf You <> criteria) $ forced AnyWindow
    ]
   where
    criteria = if a.token Brilliance >= 6 then NoRestriction else Never

instance RunMessage ChromaBlight where
  runMessage msg t@(ChromaBlight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      whenNone (treacheryInThreatAreaOf iid <> treacheryIs Cards.chromaBlight) do
        placeInThreatArea attrs iid
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Brilliance 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid $ createSetAsideEnemy_ Enemies.crystalParasite
      removeFromGame attrs
      pure t
    _ -> ChromaBlight <$> liftRunMessage msg attrs
