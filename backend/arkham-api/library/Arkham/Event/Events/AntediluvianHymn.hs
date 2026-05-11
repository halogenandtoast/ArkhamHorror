module Arkham.Event.Events.AntediluvianHymn (antediluvianHymn) where

import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.I18n
import Arkham.Strategy
import Arkham.Zone

newtype AntediluvianHymn = AntediluvianHymn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

antediluvianHymn :: EventCard AntediluvianHymn
antediluvianHymn = event AntediluvianHymn Cards.antediluvianHymn

instance RunMessage AntediluvianHymn where
  runMessage msg e@(AntediluvianHymn attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lookAt
        iid
        attrs
        EncounterDeckTarget
        [(FromTopOfDeck 5, PutBackInAnyOrder)]
        #any
        (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      n <- getRemainingCurseTokens
      focusCards cards do
        cardI18n $ scope "antediluvianHymn" $ chooseUpToNM' iid n "doNotAddCurseToken" do
          targets cards \card -> do
            addCurseTokens (Just iid) 1
            putCardOnBottomOfDeck iid Deck.EncounterDeck card
      pure e
    _ -> AntediluvianHymn <$> liftRunMessage msg attrs
