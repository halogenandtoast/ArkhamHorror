module Arkham.Location.Cards.TemploMayor_174 (
  temploMayor_174,
  TemploMayor_174 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype TemploMayor_174 = TemploMayor_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_174 :: LocationCard TemploMayor_174
temploMayor_174 =
  locationWith
    TemploMayor_174
    Cards.temploMayor_174
    4
    (PerPlayer 1)
    (labelL .~ "circle")

instance HasAbilities TemploMayor_174 where
  getAbilities (TemploMayor_174 attrs) =
    withRevealedAbilities attrs $
      [ mkAbility attrs 1 $
          ForcedAbility $
            PutLocationIntoPlay Timing.After Anyone $
              LocationWithId $
                toId attrs
      , limitedAbility (GroupLimit PerPhase 1)
          $ restrictedAbility
            attrs
            2
            ( Here
                <> CluesOnThis (AtLeast $ Static 1)
                <> CanDiscoverCluesAt
                  (LocationWithId $ toId attrs)
            )
          $ ReactionAbility
            ( OrWindowMatcher
                [ EnemyDefeated
                    Timing.After
                    You
                    ByAny
                    (enemyAt (toId attrs) <> EnemyWithTrait Serpent)
                , EnemyEvaded
                    Timing.After
                    You
                    (enemyAt (toId attrs) <> EnemyWithTrait Serpent)
                ]
            )
            Free
      ]

instance RunMessage TemploMayor_174 where
  runMessage msg l@(TemploMayor_174 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      lead <- getLead
      pushAll
        [ ShuffleEncounterDiscardBackIn
        , DiscardUntilFirst
            lead
            (toSource attrs)
            Deck.EncounterDeck
            (BasicCardMatch $ CardWithType EnemyType <> CardWithTrait Serpent)
        ]
      pure l
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      push $ SpawnEnemyAt (EncounterCard ec) (toId attrs)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 2) 1 Nothing
      pure l
    _ -> TemploMayor_174 <$> runMessage msg attrs
