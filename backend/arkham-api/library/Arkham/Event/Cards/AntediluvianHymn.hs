module Arkham.Event.Cards.AntediluvianHymn (antediluvianHymn, AntediluvianHymn (..)) where

import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Classes.HasQueue (evalQueueT)
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
      addCurse <- evalQueueT $ addCurseTokens 1
      focusCards cards \unfocus -> do
        chooseUpToN
          iid
          n
          "Do not add 1 {curse} token to place a card on the bottom of the encounter deck"
          [ targetLabel card $ addCurse <> [PutCardOnBottomOfDeck iid Deck.EncounterDeck card]
          | card <- cards
          ]
        push unfocus
      pure e
    _ -> AntediluvianHymn <$> liftRunMessage msg attrs
