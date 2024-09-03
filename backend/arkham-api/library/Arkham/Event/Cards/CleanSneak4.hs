module Arkham.Event.Cards.CleanSneak4 (cleanSneak4, CleanSneak4 (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

data Meta = Meta {optionsChosen :: [Int], enemiesChosen :: [EnemyId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CleanSneak4 = CleanSneak4 (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleanSneak4 :: EventCard CleanSneak4
cleanSneak4 = event (CleanSneak4 . (`with` Meta [] [])) Cards.cleanSneak4

instance RunMessage CleanSneak4 where
  runMessage msg e@(CleanSneak4 (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      push $ Do msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      canGainResources <- (&& (1 `notElem` optionsChosen meta)) <$> can.gain.resources iid
      canDiscoverClues <-
        (&& (3 `notElem` optionsChosen meta))
          <$> (maybe (pure False) (getCanDiscoverClues NotInvestigate iid) =<< field InvestigatorLocation iid)
      canDrawCards <- (&& (4 `notElem` optionsChosen meta)) <$> can.draw.cards iid

      enemies <-
        select
          $ enemyAtLocationWith iid
          <> not_ IsSwarm
          <> ExhaustedEnemy
          <> not_ (mapOneOf EnemyWithId (enemiesChosen meta))
          <> ( if (2 `notElem` optionsChosen meta && not (or [canGainResources, canDiscoverClues, canDrawCards]))
                then EnemyCanBeDamagedBySource (toSource attrs)
                else AnyEnemy
             )

      when (notNull enemies && length (optionsChosen meta) < 4) do
        chooseOneToHandle iid attrs enemies
        push msg
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      canGainResources <- can.gain.resources iid
      canDrawCards <- can.draw.cards iid
      canDiscoverClues <-
        maybe (pure False) (getCanDiscoverClues NotInvestigate iid) =<< field InvestigatorLocation iid
      canDamage <- eid <=~> EnemyCanBeDamagedBySource (toSource attrs)

      chooseOne iid
        $ [Label "Gain 2 resources" [DoStep 1 msg] | canGainResources && 1 `notElem` optionsChosen meta]
        <> [Label "Deal 2 damage to that Enemy" [DoStep 2 msg] | canDamage && 2 `notElem` optionsChosen meta]
        <> [ Label "Discover 1 clue at your location" [DoStep 3 msg]
           | canDiscoverClues && 3 `notElem` optionsChosen meta
           ]
        <> [Label "Draw 1 card" [DoStep 4 msg] | canDrawCards && 4 `notElem` optionsChosen meta]
      pure e
    DoStep n (HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid)) -> do
      case n of
        1 -> gainResourcesIfCan iid attrs 2
        2 -> nonAttackEnemyDamage attrs 2 eid
        3 -> discoverAtYourLocation NotInvestigate iid attrs 1
        4 -> drawCardsIfCan iid attrs 1
        _ -> error "Unhandled options"
      pure . CleanSneak4 $ attrs `with` Meta (n : optionsChosen meta) (eid : enemiesChosen meta)
    _ -> CleanSneak4 . (`with` meta) <$> liftRunMessage msg attrs
