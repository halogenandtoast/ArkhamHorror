module Arkham.Homebrew.ThePhantomOfTheOpera.Treacheries.AlwaysWatchingFeint (alwaysWatchingFeint) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers (afterExposed)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype AlwaysWatchingFeint = AlwaysWatchingFeint TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alwaysWatchingFeint :: TreacheryCard AlwaysWatchingFeint
alwaysWatchingFeint = treachery AlwaysWatchingFeint Cards.alwaysWatchingFeint

-- The route tag is raised by the shared expose helpers; see Concealed.Helpers.
instance HasAbilities AlwaysWatchingFeint where
  getAbilities (AlwaysWatchingFeint a) =
    mkAbility a 1 (forced $ CampaignEvent #after (Just You) "exposedVia[evade]")
      : [mkAbility a 2 $ forced $ RoundEnds #when | phantomExposed]
        <> [mkAbility a 3 $ SilentForcedAbility $ afterExposed Enemies.thePhantom | not phantomExposed]
   where
    phantomExposed = toResultDefault False a.meta

instance RunMessage AlwaysWatchingFeint where
  runMessage msg t@(AlwaysWatchingFeint attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      pure . AlwaysWatchingFeint $ setMeta True attrs
    _ -> AlwaysWatchingFeint <$> liftRunMessage msg attrs
