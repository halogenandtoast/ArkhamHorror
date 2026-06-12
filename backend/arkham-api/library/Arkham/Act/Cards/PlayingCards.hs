module Arkham.Act.Cards.PlayingCards (playingCards) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreationMethod (SpawnEngagedWith))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardsMatching)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Criminal))

newtype PlayingCards = PlayingCards ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

playingCards :: ActCard PlayingCards
playingCards = act (1, A) PlayingCards Cards.playingCards Nothing

instance HasModifiersFor PlayingCards where
  getModifiersFor (PlayingCards a) = do
    modifySelect a Anyone [CannotPerformAction (IsAction #resign)]
    modifySelect a (enemyIs Enemies.cloverClubPitBoss) [AddKeyword Keyword.Aloof]

instance HasAbilities PlayingCards where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ forced $ EnemyDefeated #after Anyone ByAny (EnemyWithTrait Criminal)
    , restricted a 2 (exists $ InvestigatorWithResources $ atLeast 15)
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage PlayingCards where
  runMessage msg a@(PlayingCards attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      laBellaLuna <- selectJust $ locationIs Locations.laBellaLuna
      createSetAsideEnemy_ Enemies.siobhanRiley laBellaLuna
      bouncers <- getSetAsideCardsMatching $ cardIs Enemies.cloverClubBouncer
      investigators <- getInvestigators
      for_ (zip investigators bouncers) \(iid, card) -> createEnemy_ card (SpawnEngagedWith iid)
      advanceActDeck attrs
      pure a
    _ -> PlayingCards <$> liftRunMessage msg attrs
