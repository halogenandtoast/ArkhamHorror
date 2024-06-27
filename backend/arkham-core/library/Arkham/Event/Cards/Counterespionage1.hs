module Arkham.Event.Cards.Counterespionage1 (counterespionage1, Counterespionage1 (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Modifiers (getMetaMaybe)
import Arkham.Matcher
import Arkham.Modifier

newtype Counterespionage1 = Counterespionage1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

counterespionage1 :: EventCard Counterespionage1
counterespionage1 = event Counterespionage1 Cards.counterespionage1

instance HasAbilities Counterespionage1 where
  getAbilities (Counterespionage1 a) =
    [ withTooltip
        "{reaction} When you play Counterespionage, increase its cost by 2. Change \"the encounter deck\" to \"your deck\"."
        $ restrictedAbility a 1 InYourHand
        $ ReactionAbility (PlayCard #when You (basic $ CardWithId a.cardId)) (IncreaseCostOfThis a.cardId 2)
    , withTooltip
        "{reaction} When you play Counterespionage, increase its cost by 2: Change \"you\" to \"any investigator\"."
        $ restrictedAbility a 2 (InYourHand <> can.affect.otherPlayers You)
        $ ForcedWhen (EventWindowInvestigatorIs (not_ You))
        $ ReactionAbility (PlayCard #when You (basic $ CardWithId a.cardId)) (IncreaseCostOfThis a.cardId 2)
    ]

instance RunMessage Counterespionage1 where
  runMessage msg e@(Counterespionage1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cancelRevelation attrs
      useYourDeck <- getMetaMaybe False attrs.cardId "yourDeck"
      if useYourDeck
        then drawCardsIfCan iid attrs 1
        else drawEncounterCard iid attrs
      pure e
    InHand _ (UseThisAbility _ (isSource attrs -> True) 1) -> do
      eventModifier attrs attrs.cardId (MetaModifier $ object ["yourDeck" .= True])
      pure e
    InHand _ (UseThisAbility _ (isSource attrs -> True) 2) -> pure e
    _ -> Counterespionage1 <$> lift (runMessage msg attrs)
