module Arkham.Treachery.Cards.CallOfTheUnknown
  ( callOfTheUnknown
  , CallOfTheUnknown(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.History
import Arkham.Id
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import qualified Arkham.Timing as Timing
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata { chosenLocation :: Maybe LocationId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CallOfTheUnknown = CallOfTheUnknown (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfTheUnknown :: TreacheryCard CallOfTheUnknown
callOfTheUnknown = treachery
  (CallOfTheUnknown . (`with` Metadata Nothing))
  Cards.callOfTheUnknown

instance HasAbilities CallOfTheUnknown where
  getAbilities (CallOfTheUnknown (a `With` _)) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility (TurnBegins Timing.When You)
    ]

instance RunMessage CallOfTheUnknown where
  runMessage msg t@(CallOfTheUnknown (attrs `With` metadata)) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ AttachTreachery (toId t) (InvestigatorTarget iid)
      pure t
    UseCardAbility iid source windows' 1 payment | isSource attrs source -> do
      targets <-
        selectListMap LocationTarget $ NotLocation $ locationWithInvestigator
          iid
      when (notNull targets) $ push $ chooseOne
        iid
        [ TargetLabel
            target
            [UseCardAbilityChoiceTarget iid source windows' 1 payment target]
        | target <- targets
        ]
      pure t
    UseCardAbilityChoiceTarget _ source _ 1 _ (LocationTarget lid)
      | isSource attrs source -> do
        pure $ CallOfTheUnknown $ attrs `with` Metadata (Just lid)
    When (EndTurn iid) | treacheryOnInvestigator iid attrs -> do
      -- use When here to trigger before turn history is erased
      for_ (chosenLocation metadata) $ \lid -> do
        history <- getHistory TurnHistory iid
        when (lid `notMember` historyLocationsSuccessfullyInvestigated history)
          $ pushAll
              [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
              , ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
              ]
      pure $ CallOfTheUnknown $ attrs `with` Metadata Nothing
    _ -> CallOfTheUnknown . (`with` metadata) <$> runMessage msg attrs
