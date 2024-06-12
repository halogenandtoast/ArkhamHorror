module Arkham.Location.Cards.TemploMayor_174 (temploMayor_174, TemploMayor_174 (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Trait

newtype TemploMayor_174 = TemploMayor_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_174 :: LocationCard TemploMayor_174
temploMayor_174 = locationWith TemploMayor_174 Cards.temploMayor_174 4 (PerPlayer 1) (labelL .~ "circle")

instance HasAbilities TemploMayor_174 where
  getAbilities (TemploMayor_174 attrs) =
    extendRevealed attrs
      $ [ mkAbility attrs 1 $ forced $ PutLocationIntoPlay #after Anyone (be attrs)
        , groupLimit PerPhase
            $ restrictedAbility
              attrs
              2
              (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (LocationWithId attrs.id))
            $ freeReaction
            $ oneOf
              [ EnemyDefeated #after You ByAny (enemyAt attrs.id <> EnemyWithTrait Serpent)
              , EnemyEvaded #after You (enemyAt attrs.id <> EnemyWithTrait Serpent)
              ]
        ]

instance RunMessage TemploMayor_174 where
  runMessage msg l@(TemploMayor_174 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      pushAll
        [ ShuffleEncounterDiscardBackIn
        , DiscardUntilFirst lead (toSource attrs) Deck.EncounterDeck (basic $ #enemy <> CardWithTrait Serpent)
        ]
      pure l
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      push $ SpawnEnemyAt (EncounterCard ec) (toId attrs)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 2) 1
      pure l
    _ -> TemploMayor_174 <$> runMessage msg attrs
