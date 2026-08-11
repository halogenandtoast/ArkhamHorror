module Arkham.Homebrew.DarkMatter.Treacheries.SongOfYourSoul (songOfYourSoul) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype SongOfYourSoul = SongOfYourSoul TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfYourSoul :: TreacheryCard SongOfYourSoul
songOfYourSoul = treachery SongOfYourSoul Cards.songOfYourSoul

{- | "Revelation - Put Song of Your Soul into play in your threat area.
Forced - At the end of the round: For each event in your hand, either discard it
or take 1 horror. Then, discard Song of Your Soul."
-}
instance HasAbilities SongOfYourSoul where
  getAbilities (SongOfYourSoul a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage SongOfYourSoul where
  runMessage msg t@(SongOfYourSoul attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      events <- select $ inHandOf NotForPlay iid <> basic #event
      for_ events \event -> do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled' "discardCardsFromHand" $ discardCard iid (attrs.ability 1) event
          countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SongOfYourSoul <$> liftRunMessage msg attrs
