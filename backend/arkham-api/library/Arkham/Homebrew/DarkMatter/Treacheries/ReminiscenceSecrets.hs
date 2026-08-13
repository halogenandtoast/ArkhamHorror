module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscenceSecrets (reminiscenceSecrets) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (InvestigatorEliminated)

newtype ReminiscenceSecrets = ReminiscenceSecrets TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reminiscenceSecrets :: TreacheryCard ReminiscenceSecrets
reminiscenceSecrets = treachery ReminiscenceSecrets Cards.reminiscenceSecrets

instance HasAbilities ReminiscenceSecrets where
  getAbilities (ReminiscenceSecrets a) =
    [ mkAbility a 1 $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
    , mkAbility a 2 $ freeReaction $ WouldDiscoverClues #when Anyone (here a) AnyValue
    ]

-- The card lives hidden in its holder's hand, so @inThreatAreaOf@ is Nothing and
-- scoping off it alone would leave the ability matching every location.
here :: TreacheryAttrs -> LocationMatcher
here a = case a.placement of
  HiddenInHand iid -> locationWithInvestigator iid
  InThreatArea iid -> locationWithInvestigator iid
  _ -> Anywhere

instance RunMessage ReminiscenceSecrets where
  runMessage msg t@(ReminiscenceSecrets attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "place 1 of their clues on that location instead of discovering clues"
      selectOne (here attrs) >>= traverse_ \lid -> moveTokens (attrs.ability 2) iid lid #clue 1
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ReminiscenceSecrets <$> liftRunMessage msg attrs
