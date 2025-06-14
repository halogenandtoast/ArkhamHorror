module Arkham.Location.Cards.TemploMayor_174 (temploMayor_174) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype TemploMayor_174 = TemploMayor_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_174 :: LocationCard TemploMayor_174
temploMayor_174 = symbolLabel $ location TemploMayor_174 Cards.temploMayor_174 4 (PerPlayer 1)

instance HasAbilities TemploMayor_174 where
  getAbilities (TemploMayor_174 a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)
      , groupLimit PerPhase
          $ restricted a 2 (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (be a))
          $ freeReaction
          $ oneOf
            [ EnemyDefeated #after You ByAny (enemyAt a.id <> EnemyWithTrait Serpent)
            , EnemyEvaded #after You (enemyAt a.id <> EnemyWithTrait Serpent)
            ]
      ]

instance RunMessage TemploMayor_174 where
  runMessage msg l@(TemploMayor_174 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      shuffleEncounterDiscardBackIn
      discardUntilFirst lead attrs Deck.EncounterDeck (basic $ #enemy <> CardWithTrait Serpent)
      pure l
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      spawnEnemyAt_ ec attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      discoverAt NotInvestigate iid (attrs.ability 2) attrs 1
      pure l
    _ -> TemploMayor_174 <$> liftRunMessage msg attrs
