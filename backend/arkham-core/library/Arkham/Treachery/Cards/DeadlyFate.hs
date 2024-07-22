module Arkham.Treachery.Cards.DeadlyFate (deadlyFate, DeadlyFate (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DeadlyFate = DeadlyFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlyFate :: TreacheryCard DeadlyFate
deadlyFate = treachery DeadlyFate Cards.deadlyFate

instance RunMessage DeadlyFate where
  runMessage msg t@(DeadlyFate attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ DiscardUntilFirst iid (toSource attrs) Deck.EncounterDeck $ basic #enemy
      pure t
    RequestedEncounterCard source _ mcard | isSource attrs source -> do
      iid <- selectJust You
      case mcard of
        Nothing -> push $ assignHorror iid source 1
        Just c -> do
          -- tricky, we must create an enemy that has been discaded, have it
          -- attack,  and then remove it
          -- This technically means we have an enemy at no location
          player <- getPlayer iid
          pushAll
            [ FocusCards [EncounterCard c]
            , chooseOne
                player
                [ Label "Draw enemy" [InvestigatorDrewEncounterCard iid c]
                , Label
                    "That enemy attacks you (from the discard pile)"
                    [ AddToEncounterDiscard c
                    , EnemyAttackFromDiscard iid (toSource attrs) (EncounterCard c)
                    ]
                ]
            , UnfocusCards
            ]
      pure t
    _ -> DeadlyFate <$> runMessage msg attrs
