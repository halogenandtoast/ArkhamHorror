module Arkham.Homebrew.ThePhantomOfTheOpera.Treacheries.AngelOfMusic (angelOfMusic) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (
  ForExpose (NotForExpose),
  getConcealedAtAll,
 )
import Arkham.Campaigns.TheScarletKeys.Helpers (afterExposed)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEachMapM)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype AngelOfMusic = AngelOfMusic TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

angelOfMusic :: TreacheryCard AngelOfMusic
angelOfMusic = treachery AngelOfMusic Cards.angelOfMusic

instance HasModifiersFor AngelOfMusic where
  getModifiersFor (AngelOfMusic a) = when (a.placement == NextToAgenda) do
    iids <- select Anyone
    modifyEachMapM a iids \iid -> do
      n <- maybe (pure 0) (fmap length . getConcealedAtAll NotForExpose) =<< getLocationOf iid
      pure [HandSize (-n) | n > 0]

instance HasAbilities AngelOfMusic where
  getAbilities (AngelOfMusic a) =
    [mkAbility a 1 $ forced $ RoundEnds #when | phantomExposed]
      <> [mkAbility a 2 $ SilentForcedAbility $ afterExposed Enemies.thePhantom | not phantomExposed]
   where
    phantomExposed = toResultDefault False a.meta

instance RunMessage AngelOfMusic where
  runMessage msg t@(AngelOfMusic attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      inPlay <- selectAny $ treacheryIs Cards.angelOfMusic <> TreacheryWithPlacement NextToAgenda
      if inPlay then gainSurge attrs else placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure . AngelOfMusic $ setMeta True attrs
    _ -> AngelOfMusic <$> liftRunMessage msg attrs
